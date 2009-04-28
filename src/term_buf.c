/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#include "eep0018.h"
#include "term_buf.h"

#define DOUBLE_SLAB_SIZE (4096 - sizeof(double_slab))
#define DOUBLES_PER_SLAB ((int)(sizeof(double) / DOUBLE_SLAB_SIZE))

#define INIT_TERM_BUF_SIZE  16

#define GROW_BUF(BUF, N) { if (term_buf_grow(BUF, N) == NULL) return ERROR; }

// Internal function to make a copy of a double for passing into driver_send_term
static double*
term_buf_copy_double(term_buf* buf, double val)
{
    if (buf->doubles == NULL || (buf->doubles->size - buf->doubles->used == 0))
    {
        // Need to allocate a new slab
        double_slab* slab = driver_alloc(DOUBLE_SLAB_SIZE);
        memset(slab, '\0', DOUBLE_SLAB_SIZE);
        slab->size = DOUBLES_PER_SLAB;
        slab->values = (double*)(slab + sizeof(double_slab));
        slab->next_slab = buf->doubles;
        buf->doubles = slab;
    }

    int used = buf->doubles->used;
    buf->doubles->values[buf->doubles->used++] = val;
    return buf->doubles->values + used;
}


// Internal function to grow array of terms 
static ErlDrvTermData*
term_buf_grow(term_buf* buf, int elements)
{
    if (buf->terms_len - buf->terms_used < elements)
    {
        buf->terms_len *= 2;
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
    memset(buf, '\0', sizeof(term_buf));

    // Initialize atom values that we re-use heavily
    buf->true_atom  = driver_mk_atom("true");
    buf->false_atom = driver_mk_atom("false");
    buf->null_atom  = driver_mk_atom("null");

    // Allocate array to hold erlang message we will construct. We do this
    // for efficiency reasons -- there will be at least one term in every 
    // message.
    buf->terms_len = INIT_TERM_BUF_SIZE;
    buf->terms = (ErlDrvTermData*) driver_alloc(buf->terms_len * sizeof(ErlDrvTermData));
    if(buf->terms == NULL)
    {
        return -1;
    }

    return 0;
}

void
term_buf_destroy(term_buf* buf)
{
    // Walk list of double_slab and free each one
    double_slab* d = buf->doubles;
    while (d != NULL)
    {
        double_slab* dnext = d->next_slab;
        driver_free(d);
        d = dnext;
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
    GROW_BUF(buf, 2);

    double* ptr = term_buf_copy_double(buf, value);
    if (ptr == NULL)
    {
        return ERROR;
    }
    
    buf->terms[buf->terms_used++] = ERL_DRV_FLOAT;
    buf->terms[buf->terms_used++] = (ErlDrvTermData)ptr;
    return OK;
}
