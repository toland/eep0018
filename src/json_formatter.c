/* This is based on json_reformat.c from YAJL which contained the following
 * copyright notice.
 */
/*
 * Copyright 2007-2009, Lloyd Hilaiel.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 * 
 *  3. Neither the name of Lloyd Hilaiel nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */ 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eep0018.h"
#include "yajl_gen.h"

static int reformat_null(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_null(g);
    return 1;
}

static int reformat_boolean(void * ctx, int boolean)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_bool(g, boolean);
    return 1;
}

static int reformat_number(void * ctx, const char * s, unsigned int l)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_number(g, s, l);
    return 1;
}

static int reformat_string(void * ctx, const unsigned char * stringVal,
                           unsigned int stringLen)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_string(g, stringVal, stringLen);
    return 1;
}

static int reformat_map_key(void * ctx, const unsigned char * stringVal,
                            unsigned int stringLen)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_string(g, stringVal, stringLen);
    return 1;
}

static int reformat_start_map(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_map_open(g);
    return 1;
}


static int reformat_end_map(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_map_close(g);
    return 1;
}

static int reformat_start_array(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_array_open(g);
    return 1;
}

static int reformat_end_array(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    yajl_gen_array_close(g);
    return 1;
}

static yajl_callbacks callbacks = {
    reformat_null,
    reformat_boolean,
    NULL,
    NULL,
    reformat_number,
    reformat_string,
    reformat_start_map,
    reformat_map_key,
    reformat_end_map,
    reformat_start_array,
    reformat_end_array
};

void reformat(ErlDrvPort port, char* buf, int len)
{
    yajl_handle hand;
    /* generator config */
    yajl_gen_config conf = { 1, "  " };
	yajl_gen g;
    yajl_status stat;
    /* allow comments */
    yajl_parser_config cfg = { 1, 1 };

    g = yajl_gen_alloc(&conf, NULL);

    /* ok.  open file.  let's read and parse */
    hand = yajl_alloc(&callbacks, &cfg, NULL, (void *) g);
    
    /* read file data, pass to parser */
    stat = yajl_parse(hand, (unsigned char*) buf, len);

    if (stat != yajl_status_ok &&
        stat != yajl_status_insufficient_data)
    {
        char* err = (char*) yajl_get_error(hand, 1, (unsigned char*) buf, len);
        int len = strlen(err);

        ErlDrvTermData msg[] = {
            ERL_DRV_ATOM, driver_mk_atom("error"), 
            ERL_DRV_BUF2BINARY, (ErlDrvTermData) err, (ErlDrvUInt) len,
            ERL_DRV_TUPLE, 2
        };

        driver_send_term(port, driver_caller(port), msg, sizeof(msg) / sizeof(msg[0]));
    } else {
        const unsigned char* json;
        unsigned int len;
        yajl_gen_get_buf(g, &json, &len);

        ErlDrvTermData msg[] = {
            ERL_DRV_ATOM, driver_mk_atom("ok"),
            ERL_DRV_BUF2BINARY, (ErlDrvTermData) json, (ErlDrvUInt) len,
            ERL_DRV_TUPLE, 2
        };

        driver_send_term(port, driver_caller(port), msg, sizeof(msg) / sizeof(msg[0]));

        yajl_gen_clear(g);
    }

    yajl_gen_free(g);
    yajl_free(hand);
}
