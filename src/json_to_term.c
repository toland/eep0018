/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#include "eep0018.h"
#include "term_buf.h"

#ifndef WIN32
#include <string.h>
#endif

#define YAJL_OK 1
#define YAJL_ERROR 0

#define CHECK_CALL(CALL) if((CALL)) return YAJL_ERROR;

typedef struct
{
    term_buf* buf;
    int       state[MAX_DEPTH];
    int       depth;
} State;

static State*
prepare(void* ctx)
{
    State* st = (State*) ctx;

    if(st->depth > 0 && st->state[st->depth] >= 0)
    {
        st->state[st->depth]++;
    }

    return st;
}

static int
finish(State* st)
{
    if(st->depth > 0 && st->state[st->depth] < 0)
    {
        CHECK_CALL(term_buf_tuple(st->buf, 2));
        st->depth--;
    }

    return YAJL_OK;
}

static int
erl_json_null(void* ctx)
{
    State* st = prepare(ctx);
    CHECK_CALL(term_buf_null(st->buf));
    return finish(st);
}

static int
erl_json_boolean(void* ctx, int boolVal)
{
    State* st = prepare(ctx);
    
    if(boolVal)
    {
        CHECK_CALL(term_buf_true(st->buf));
    }
    else
    {
        CHECK_CALL(term_buf_false(st->buf));
    }

    return finish(st);
}

static int
erl_json_long(void* ctx, long val)
{
    State* st = prepare(ctx);
    CHECK_CALL(term_buf_int(st->buf, val));
    return finish(st);
}

static int
erl_json_double(void* ctx, double val)
{
    State* st = prepare(ctx);
    CHECK_CALL(term_buf_double(st->buf, val));
    return finish(st);
}

static int
erl_json_string(void* ctx, const unsigned char * stringVal, unsigned int stringLen)
{
    State* st = prepare(ctx);
    CHECK_CALL(term_buf_binary(st->buf, stringVal, stringLen));
    return finish(st);
}
 
static int
erl_json_start_map(void* ctx)
{
    // {"foo": 1} -> {[{<<"foo">>, 1}]}
    State* st = prepare(ctx);
    st->state[++st->depth] = 0;
    return YAJL_OK;
}

static int
erl_json_end_map(void* ctx)
{
    State* st = (State*) ctx;
    //Close the list of two tuples
    CHECK_CALL(term_buf_list(st->buf, st->state[st->depth--]));
    //Close the 1 tuple enclosure
    CHECK_CALL(term_buf_tuple(st->buf, 1));
    return finish(st);
}

static int
erl_json_map_key(void* ctx, const unsigned char* keyVal, unsigned int keyLen)
{
    State* st = prepare(ctx);
    st->state[++st->depth] = -1;
    CHECK_CALL(term_buf_binary(st->buf, keyVal, keyLen));
    return YAJL_OK;
}

static int
erl_json_start_array(void* ctx)
{
    State* st = prepare(ctx);
    st->state[++st->depth] = 0;
    return YAJL_OK;
}

static int
erl_json_end_array(void* ctx)
{
    State* st = (State*) ctx;
    CHECK_CALL(term_buf_list(st->buf, st->state[st->depth--]));
    return finish(st);
}

static yajl_callbacks erl_json_callbacks = {
    erl_json_null,
    erl_json_boolean,
    erl_json_long,
    erl_json_double,
    NULL,
    erl_json_string,
    erl_json_start_map,
    erl_json_map_key,
    erl_json_end_map,
    erl_json_start_array,
    erl_json_end_array
};

#define ALLOW_COMMENTS 0
#define CHECK_UTF8 0

int
json_to_term(ErlDrvPort port, char* buf, int len)
{
    term_buf tbuf;
    State st;
    st.depth = 0;
    st.state[st.depth] = 0;
    st.buf = &tbuf;

    // Attempt to allocate the term buffer; if it fails we kill this port instance. 
    if (term_buf_init(&tbuf) == -1)
    {
        driver_failure_atom(port, "Allocation error");
        return 0;
    }

    // Go ahead and push the beginning of the response into the term buffer. If the parse
    // fails we won't be using this for our message, so it's safe to do.
    term_buf_add2(&tbuf, ERL_DRV_ATOM, driver_mk_atom("json_ok"));

    // Setup to parse and then execute the parse
    yajl_parser_config conf = {ALLOW_COMMENTS, CHECK_UTF8};
    yajl_handle handle = yajl_alloc(&erl_json_callbacks, &conf, &st);

    if (yajl_parse(handle, (unsigned char*) buf, len) == yajl_status_ok)
    {
        // Parse was successful, wrap up the tuple we started with and send it on
        term_buf_tuple(st.buf, 2);
        driver_send_term(port, driver_caller(port), st.buf->terms, st.buf->terms_used);
    }
    else
    {
        // Parse failed -- send message back to caller with whatever the parser can tell
        // us for context.
        unsigned char* msg;
        msg = yajl_get_error(handle, 0, NULL, 0);

        ErlDrvTermData response[] = { ERL_DRV_ATOM, driver_mk_atom("json_error"),
                                      ERL_DRV_ATOM, driver_mk_atom("parse_error"),
                                      ERL_DRV_BUF2BINARY, (ErlDrvTermData) msg, strlen((char*) msg),
                                      ERL_DRV_TUPLE, 2,
                                      ERL_DRV_TUPLE, 2 };

        driver_send_term(port, driver_caller(port), response, sizeof(response) / sizeof(response[0]));

        yajl_free_error(msg);
    }

    // Regardless of what happens, clean up everything
    yajl_free(handle);
    term_buf_destroy(&tbuf);
    return 0;
}
