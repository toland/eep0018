/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#include "eep0018.h"
#include "term_buf.h"

#include <assert.h>

#define SLAB_SIZE      4000
#define SLAB_DATA_SIZE SLAB_SIZE - sizeof(slab)

#define INIT_TERM_BUF_SIZE  16

#define GROW_BUF(BUF, N) { if (term_buf_grow(BUF, N) == NULL) return ERROR; }

// Internal function to allocate a room on a slab
static void*
term_buf_slab_alloc(term_buf* buf, int size)
{
    if (buf->slabs->used + size > SLAB_DATA_SIZE)
    {
        // Determine if the requested data will fit on our default slab; if it won't,
        // allocate a specially sized slab and hand it back.
        int alloc_size = SLAB_SIZE;
        if (size > SLAB_DATA_SIZE)
        {
            alloc_size = size + sizeof(slab);
        }

        slab* new_slab = driver_alloc(alloc_size);
        new_slab->used = 0;
        new_slab->next_slab = buf->slabs;
        buf->slabs = new_slab;
    }

    int offset = buf->slabs->used;
    buf->slabs->used += size;
    return buf->slabs->data + offset;
}


// Internal function to grow array of terms 
static ErlDrvTermData*
term_buf_grow(term_buf* buf, int elements)
{
    if (buf->terms_len - buf->terms_used < elements)
    {
        // Growing the terms_len by 2 should always be enough for our purposes since
        // the initial allocation size is 16 and we only add 2-3 elements at a time. However,
        // future changes could introduce an issue, so we add an assert.
        buf->terms_len *= 2;
        assert(buf->terms_len - buf->terms_used >= elements);
        buf->terms = (ErlDrvTermData*) driver_realloc(buf->terms, buf->terms_len * sizeof(ErlDrvTermData));
        if (buf->terms == NULL)
        {
            return NULL;
        }
    }

    return buf->terms;
}


int
term_buf_init(term_buf* buf)
{
    // Initialize atom values that we re-use heavily
    buf->true_atom  = driver_mk_atom("true");
    buf->false_atom = driver_mk_atom("false");
    buf->null_atom  = driver_mk_atom("null");

    // Allocate array to hold erlang message we will construct. We do this
    // for efficiency reasons -- there will be at least one term in every 
    // message.
    buf->terms_len  = INIT_TERM_BUF_SIZE;
    buf->terms_used = 0;
    buf->terms = (ErlDrvTermData*) driver_alloc(buf->terms_len * sizeof(ErlDrvTermData));
    if(buf->terms == NULL)
    {
        return -1;
    }

    // Go ahead and allocate initial slab
    slab* new_slab = driver_alloc(SLAB_SIZE);
    new_slab->used = 0;
    new_slab->next_slab = 0;
    buf->slabs = new_slab;

    return 0;
}

void
term_buf_destroy(term_buf* buf)
{
    // Walk list of slabs and free each one
    slab* s = buf->slabs;
    while (s != NULL)
    {
        slab* snext = s->next_slab;
        driver_free(s);
        s = snext;
    }
    driver_free(buf->terms);
}


int
term_buf_add2(term_buf* buf, ErlDrvTermData d1, ErlDrvTermData d2)
{
    GROW_BUF(buf, 2);
    buf->terms[buf->terms_used++] = d1;
    buf->terms[buf->terms_used++] = d2;
    return OK;
}

int
term_buf_add3(term_buf* buf, ErlDrvTermData d1, ErlDrvTermData d2, ErlDrvTermData d3)
{
    GROW_BUF(buf, 3);
    buf->terms[buf->terms_used++] = d1;
    buf->terms[buf->terms_used++] = d2;
    buf->terms[buf->terms_used++] = d3;
    return OK;
}

int
term_buf_double(term_buf* buf, double value)
{
    double* ptr = (double*)term_buf_slab_alloc(buf, sizeof(double));
    *ptr = value;
    return term_buf_add2(buf, ERL_DRV_FLOAT, (ErlDrvTermData)ptr);
}

int
term_buf_binary(term_buf* buf, const unsigned char* data, int len)
{
    char* data_copy = (char*)term_buf_slab_alloc(buf, len);
    memcpy(data_copy, data, len);

    return term_buf_add3(buf, ERL_DRV_BUF2BINARY, (ErlDrvTermData)data_copy, len);
}
