/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#ifndef __EEP0018_H__
#define __EEP0018_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>

#include "yajl_parse.h"

#define ERROR 1
#define OK    0

void term_to_json(ErlDrvPort port, char* buf, int len);
int json_to_term(ErlDrvPort port, char* buf, int len);

#endif
