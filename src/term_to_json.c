/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#include "eep0018.h"

#ifndef WIN32
#include <string.h>
#endif

#ifdef DEBUG
#  define DBG printf
#else
#  define DBG(arg1,...)
#endif

// Default buffer size when allocating a buffer to hold the generate JSON. We use an
// approximate 4k buffer size to better align with page size while leaving some room
// for ERTS allocator overhead. 
#define DEFAULT_BUFFER_SIZE 4000 

typedef enum
{
    gen_start,
    gen_map_start,
    gen_map_key,
    gen_map_val,
    gen_array_start,
    gen_in_array,
    gen_complete
} gen_state;

typedef struct
{
    ErlDrvPort    port;
    ErlDrvBinary* bin;
    char*         buffer;
    int           buffer_avail;
    unsigned int  depth;
    gen_state   state_stack[YAJL_MAX_DEPTH];
} gen_ctx;

typedef union 
{
    long lval;
    double dval;
    long long llval;  
} number_union;

#define INSERT_SEP  {                                           \
        gen_state state = ctx->state_stack[ctx->depth];          \
        if (state == gen_map_key || state == gen_in_array)        \
            append_data(ctx, ",", 1);                           \
        else if (state == gen_map_val)                           \
            append_data(ctx, ":", 1);                           \
    }

#define ENSURE_NOT_KEY {                                        \
        if (ctx->state_stack[ctx->depth] == gen_map_key)        \
            return ERROR;                                       \
    }

#define INC_DEPTH {                             \
        if (++(ctx->depth) >= YAJL_MAX_DEPTH)   \
            return ERROR;                       \
    }

#define CHECK_DECODE(DFN) { if (DFN != 0) return ERROR; }
#define CHECK(C) { if (!(C))  return ERROR; }

static int value_to_json(gen_ctx* ctx, char* buf, int* index);

void flush_data(gen_ctx* ctx, int refill)
{
    int count = ctx->bin->orig_size - ctx->buffer_avail;
    DBG("flush_data: refill %d count %d\n", refill, count);
    driver_output_binary(ctx->port, 0, 0, ctx->bin, 0, count);
    if (refill)
    {
        ctx->bin    = driver_alloc_binary(DEFAULT_BUFFER_SIZE);
        ctx->buffer = ctx->bin->orig_bytes;
        ctx->buffer_avail = DEFAULT_BUFFER_SIZE;
    }
    else
    {
        ctx->bin = 0;
        ctx->buffer = 0;
        ctx->buffer_avail = 0;
    }
}

void append_data(gen_ctx* ctx, const char* str, int str_len)
{
    DBG("append_data(%d): %s\n", str_len, str);
    const char* curr = str;
    const char* str_end = str + str_len;
    while (curr != str_end)
    {
        // Start with assumption we will copy as much data as possible
        int count = str_end - curr;

        // Check for full buffer; request refill after flush
        if (ctx->buffer_avail == 0)
        {
            flush_data(ctx, 1);
        }

        // Only copy as much data as we have room for in the buffer
        if (ctx->buffer_avail < count)
        {
            count = ctx->buffer_avail;
        }

        // Copy data into the buffer
        memcpy(ctx->buffer, curr, count);
        
        // Update curr and gen_ctx 
        curr += count;
        ctx->buffer += count;
        ctx->buffer_avail -= count;
    }
}

static const char* HEXCHARS = "0123456789ABCDEF";

void append_string(gen_ctx* ctx, const char* str, int str_len)
{
    unsigned int beg = 0;
    unsigned int end = 0;    
    char hexbuf[7] = { "\\u00\0\0\0" };
    unsigned char curr;

    append_data(ctx, "\"", 1);

    while (end < str_len) 
    {
        char* escaped = NULL;
        curr = str[end];
        switch (curr) 
        {
            case '\r': escaped = "\\r"; break;
            case '\n': escaped = "\\n"; break;
            case '\\': escaped = "\\\\"; break;
            case '"' : escaped = "\\\""; break;
            case '\f': escaped = "\\f"; break;
            case '\b': escaped = "\\b"; break;
            case '\t': escaped = "\\t"; break;
            default:
                if (curr < 32)
                {
                    hexbuf[4] = HEXCHARS[curr >> 4];
                    hexbuf[5] = HEXCHARS[curr & 0x0F];
                    escaped = hexbuf;
                }
        }

        if (escaped != NULL) 
        {
            append_data(ctx, str + beg, end - beg);
            append_data(ctx, escaped, strlen(escaped));
            beg = ++end;
        } 
        else 
        {
            ++end;
        }
    }

    append_data(ctx, str + beg, end - beg);
    append_data(ctx, "\"", 1);
}


void next_state(gen_ctx* ctx)
{
    gen_state state = ctx->state_stack[ctx->depth];
    switch(state)
    {
    case gen_start:
        state = gen_complete;
        break;
    case gen_map_start:
    case gen_map_key:
        state = gen_map_val;
        break;
    case gen_array_start:
        state = gen_in_array;
        break;
    case gen_map_val:
        state = gen_map_key;
        break;
    default:
        break;
    }

    ctx->state_stack[ctx->depth] = state;
    DBG("next_state post: depth %d state %d\n", ctx->depth, ctx->state_stack[ctx->depth]);
}    


int number_to_json(gen_ctx* ctx, int type, char* buf, int* index)
{
    number_union num;
    char strbuf[32];
    int strbuf_used = 0;

    DBG("number_to_json: depth %d type %d\n", ctx->depth, type);

    ENSURE_NOT_KEY; INSERT_SEP;

    switch(type)
    {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
        CHECK_DECODE(ei_decode_long(buf, index, &num.lval));
        strbuf_used = snprintf(strbuf, sizeof(strbuf), "%ld", num.lval);
        break;

    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
        CHECK_DECODE(ei_decode_longlong(buf, index, &num.llval));
        strbuf_used = snprintf(strbuf, sizeof(strbuf), "%lld", num.llval);
        break;

    case ERL_FLOAT_EXT:
        CHECK_DECODE(ei_decode_double(buf, index, &num.dval));
        strbuf_used = snprintf(strbuf, sizeof(strbuf), "%0.20e", num.dval);
        break;

    default:
        return ERROR;
    }

    append_data(ctx, strbuf, strbuf_used);
    next_state(ctx);
    return OK;
}


int binary_to_json(gen_ctx* ctx, int size, char* buf, int* index)
{    
    DBG("binary_to_json: depth %d size %d\n", ctx->depth, size);

    INSERT_SEP;

    // One unfortunate problem with using ei_decode_binary directly is that it requires
    // a contiguous buffer to copy the binary into. We'd like to avoid this extra copy
    // cycle, so digging into the external term format, we see that the first 5 bytes of
    // the binary are header information -- thus we can skip that and access the binary
    // data that immediately follows it.

    // Append the binary data as a string (thus performing any necessary encoding)
    append_string(ctx, buf + (*index) + 5, size);
    *index += (5 + size);
    next_state(ctx);
    return OK;
}

int estring_to_json(gen_ctx* ctx, int size, char* buf, int* index)
{
    DBG("estring_to_json: depth %d size %d\n", ctx->depth, size);

    INSERT_SEP;

    // When converting a erlang "string" (which is just a list) we have no way
    // of knowing if it's actually a string or just a list of small integers. Accordingly,
    // we'll just convert the string into an array of small integers to ensure the highest
    // possible conversion fidelity.
    
    // As with ei_decode_binary, ei_decode_string requires a target buffer to copy the
    // string into. Again, we read the erl_interface source to find that in this situation
    // the first 3 bytes are header info, so we'll skip that.
    const char* data = buf + (*index) + 3;

    // Open the array
    append_data(ctx, "[", 1);

    // Loop over each char in the string and treat as integer
    char strbuf[32];
    int strbuf_used, i;
    for (i = 0; i < size; i++)
    {
        strbuf_used = snprintf(strbuf, sizeof(strbuf), "%ld", (long int)*(data + i));
        append_data(ctx, strbuf, strbuf_used);

        // Add sep if we are not at the end
        if (i < size-1)
        {
            append_data(ctx, ",", 1);
        }
    }

    // Close array
    append_data(ctx, "]", 1);

    *index += (3 + size);
    next_state(ctx);
    return OK;
}


static int atom_to_json(gen_ctx* ctx, int size, char* buf, int* index)
{
    DBG("atom_to_json: depth %d size %d\n", ctx->depth, size);

    char data[MAXATOMLEN];
    CHECK_DECODE(ei_decode_atom(buf, index, data));

    INSERT_SEP;

    // Convert known atoms to appropriate JSON constants
    if((strcmp(data, "true") == 0) || 
       (strcmp(data, "false") == 0) || 
       (strcmp(data, "null") == 0))
    {
        ENSURE_NOT_KEY;
        append_data(ctx, data, size);
    }
    else
    {
        // Unknown atom, just treat it as a normal string
        append_string(ctx, data, size);
    }
    
    return OK;
}


static int key_to_json(gen_ctx* ctx, char* buf, int* index)
{
    int type, size;

    CHECK_DECODE(ei_get_type(buf, index, &type, &size));
    DBG("key_to_json: depth %d type %d size %d\n", ctx->depth, type, size);

    if(type == ERL_BINARY_EXT)
    {
        return binary_to_json(ctx, size, buf, index);
    }
    else if(type == ERL_ATOM_EXT)
    {
        return atom_to_json(ctx, size, buf, index);
    }
    else if(type == ERL_STRING_EXT)
    {
        // In every other situation we treat an erlang "string" as an array of small 
        // integers. However, JSON only permits strings for map key values, so it's a safe
        // bet that this is actually a string. If it's not...well we assume the JSON decoder
        // at the other end can deal with UTF chars. :)

        // As noted in estring_to_json, the first 3 bytes of the buffer are header info.
        append_string(ctx, buf + (*index) + 3, size);
        *index += (3 + size);
        next_state(ctx);
        return OK;
    }
    
    return ERROR;
}


static int map_to_json(gen_ctx* ctx, char* buf, int* index)
{
    DBG("map_to_json: depth %d\n", ctx->depth);

    int i, arity;

    // Per the EEP 18 spec, tuples are only permitted as elements of lists
    // that will be converted to JSON objects. So, this tuple can only have one item and
    // it better be a list of tuples.
    CHECK_DECODE(ei_decode_tuple_header(buf, index, &arity));
    CHECK(arity == 1);

    // Check invariants and move deeper into the stack
    ENSURE_NOT_KEY; INSERT_SEP;
    INC_DEPTH;

    ctx->state_stack[ctx->depth] = gen_map_start;

    // Decode the list of tuples, where each tuple can only have an arity of 2
    CHECK_DECODE(ei_decode_list_header(buf, index, &arity));
    append_data(ctx, "{", 1);
    for(i = 0 ; i < arity ; i++)
    {
        int tuple_size = 0;
        CHECK_DECODE(ei_decode_tuple_header(buf, index, &tuple_size));
        if (tuple_size == 2)
        {
            CHECK(key_to_json(ctx, buf, index) == OK);
            CHECK(value_to_json(ctx, buf, index) == OK);
        }
        else
        {
            return ERROR;
        }
    }

    // Lists are always terminated with an empty list, but we don't really
    // want that to show up in the generated JSON. If this list is not empty,
    // skip over the empty list tail. 
    if (arity > 0)
    {
        CHECK_DECODE(ei_decode_list_header(buf, index, &arity));
        CHECK(arity == 0);
    }

    append_data(ctx, "}", 1);
    ctx->depth--;
    next_state(ctx);
    return OK;
}

static int array_to_json(gen_ctx* ctx, char* buf, int* index)
{
    DBG("array_to_json: depth %d\n", ctx->depth);

    ENSURE_NOT_KEY; INSERT_SEP; 
    INC_DEPTH;

    ctx->state_stack[ctx->depth] = gen_array_start;
    append_data(ctx, "[", 1);

    int i, arity;
    CHECK_DECODE(ei_decode_list_header(buf, index, &arity));
    for (i = 0; i < arity; i++)
    {
        CHECK(value_to_json(ctx, buf, index) == OK);
    }

    // Lists are always terminated with an empty list, but we don't really
    // want that to show up in the generated JSON. If this list is not empty,
    // skip over the empty list tail. 
    if (arity > 0)
    {
        CHECK_DECODE(ei_decode_list_header(buf, index, &arity));
        CHECK(arity == 0);
    }

    append_data(ctx, "]", 1);
    ctx->depth--;
    next_state(ctx);
    return OK;
}



static int value_to_json(gen_ctx* ctx, char* buf, int* index)
{
    int type, size;

    CHECK_DECODE(ei_get_type(buf, index, &type, &size));
    DBG("value_to_json: depth %d type %d size %d\n", ctx->depth, type, size);
    
    switch(type)
    {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
    case ERL_FLOAT_EXT:
        return number_to_json(ctx, type, buf, index);

    case ERL_ATOM_EXT:
        return atom_to_json(ctx, size, buf, index);

    case ERL_SMALL_TUPLE_EXT:
        return map_to_json(ctx, buf, index);

    case ERL_NIL_EXT:
    case ERL_LIST_EXT:
        return array_to_json(ctx, buf, index);

    case ERL_STRING_EXT:
        return estring_to_json(ctx, size, buf, index);

    case ERL_BINARY_EXT:
        return binary_to_json(ctx, size, buf, index);
    }

    return ERROR;
}


static void send_msg(ErlDrvPort port, char* atom)
{
    ErlDrvTermData msg[] = { ERL_DRV_ATOM, driver_mk_atom(atom) };
    driver_send_term(port, driver_caller(port), msg, sizeof(msg) / sizeof(msg[0]));        
}

void term_to_json(ErlDrvPort port, char* buf, int len)
{
    int index = 0;

    // First part of any term must be version info; if this decode fails
    // there is nothing more to be done.
    int version;
    if (ei_decode_version(buf, &index, &version) != 0)
    {
        send_msg(port, "term_to_json_error");
        return;
    }

    // Initialize ctx structure
    gen_ctx ctx;
    ctx.port = port;
    ctx.bin = driver_alloc_binary(DEFAULT_BUFFER_SIZE);
    ctx.buffer = ctx.bin->orig_bytes;
    ctx.buffer_avail = ctx.bin->orig_size;
    ctx.depth = 0;
    ctx.state_stack[0] = gen_start;

    // Start the conversion
    char* result_msg = "term_to_json_complete";
    if (value_to_json(&ctx, buf, &index) != OK)
    {        
        result_msg = "term_to_json_error";
    }

    // Always flush information sitting in the current buffer so that we a.) cleanup and
    // b.) have some record of where the conversion failed. It's one extra message that
    // could be avoided theoretically, but knowing where the conversion fails (roughly) is
    // useful in debug situations.
    flush_data(&ctx, 0);
    send_msg(ctx.port, result_msg);
}
