/* Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
 * Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
 *
 * This file is part of EEP0018, which is released under the MIT
 * license.
 */

#include "eep0018.h"

static ErlDrvData eep0018_start(ErlDrvPort port, char *buff)
{
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData) port;
}

static int eep0018_control(ErlDrvData drv_data,
                           unsigned int command,
                           char* buf,
                           int len,
                           char **rbuf,
                           int rlen)
{
    ErlDrvPort port = (ErlDrvPort)drv_data;

    switch(command)
    {
        case 0:
            term_to_json(port, buf, len);
            break;
        case 1:
            json_to_term(port, buf, len);
            break;
        case 2:
            reformat(port, buf, len);
            break;
    }

    *rbuf = 0;
    return 0;
}

static ErlDrvEntry eep0018_driver_entry =
{
    NULL,               /* Init */
    eep0018_start,
    NULL,               /* Stop */
    NULL,               /* Output */
    NULL,               /* Input Ready */
    NULL,               /* Output Ready */
    "eep0018_drv",      /* Driver Name */
    NULL,               /* Finish */
    NULL,               /* Handle */
    eep0018_control,
    NULL,               /* Timeout */
    NULL,               /* Outputv */
    NULL,               /* Ready Async */
    NULL,               /* Flush */
    NULL,               /* Call */
    NULL,               /* Event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,               /* Reserved */
    NULL,               /* Process Exit */
};

DRIVER_INIT(eep0018_drv)    /* must match name in driver_entry */
{
    return &eep0018_driver_entry;
}
