#ifndef _DOCOMMAND_H_
#define _DOCOMMAND_H_

extern void docommandv(const char *command, char *argv[]);
extern void docommandvp(const char *command, char *argv[]);

#define docommandl(_command, _argv...) \
do                                     \
{                                      \
    char *argv[] = { _argv };          \
    docommandv(_command, argv);        \
} while (0);

#define docommandlp(_command, _argv...) \
do                                     \
{                                      \
    char *argv[] = { _argv };          \
    docommandvp(_command, argv);        \
} while (0);

#endif /* !_DOCOMMAND_H */
