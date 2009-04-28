/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#ifndef __TERM_BUF_H__
#define __TERM_BUF_H__

#include <erl_driver.h>

typedef struct
{
    ErlDrvTermData* terms;
    int             terms_len;
    int             terms_used;
    double*         doubles;
    int             doubles_used;
    int             doubles_len;
    ErlDrvTermData  true_atom;
    ErlDrvTermData  false_atom;
    ErlDrvTermData  null_atom;
} term_buf;

int  term_buf_init(term_buf* buf);
void term_buf_destroy(term_buf* buf);

int term_buf_add2(term_buf* buf, ErlDrvTermData d1, ErlDrvTermData d2);
int term_buf_add3(term_buf* buf, ErlDrvTermData d1, ErlDrvTermData d2, ErlDrvTermData d3);

#define term_buf_tuple(buf, elements) term_buf_add2(buf, ERL_DRV_TUPLE, elements)
#define term_buf_list(buf, elements)  term_buf_add3(buf, ERL_DRV_NIL, ERL_DRV_LIST, elements+1)

#define term_buf_binary(buf, data, len) term_buf_add3(buf, ERL_DRV_BUF2BINARY, (ErlDrvTermData)data, len)

#define term_buf_true(buf)  term_buf_add2(buf, ERL_DRV_ATOM, buf->true_atom)
#define term_buf_false(buf) term_buf_add2(buf, ERL_DRV_ATOM, buf->false_atom)
#define term_buf_null(buf)  term_buf_add2(buf, ERL_DRV_ATOM, buf->null_atom)

#define term_buf_int(buf, value) term_buf_add2(buf, ERL_DRV_INT, (ErlDrvSInt)value)

int term_buf_double(term_buf* buf, double value);

#endif
