
#include <stdio.h>
#include <stdlib.h>

#ifndef __COMPCERT__
# define __builtin_ais_annot(X) do {} while(0)
#endif

void exit_ok(void)
{
    __builtin_ais_annot("instruction %here assert reachable: true;");

    exit(EXIT_SUCCESS);
}

void exit_evil(int status)
{
#   ifdef ENABLE_PRINT_ERROR_STATUS
    printf("Test %d failed.\n", status);
#   endif

#   ifndef DISABLE_STATUS_MASKING
    if (status & 0xff == EXIT_SUCCESS) {
        status = EXIT_FAILURE;
    }
#   endif

#   ifndef DISABLE_ASSERT_REACHABLE_FALSE
    __builtin_ais_annot("instruction %here assert reachable: false;");
#   endif

    exit(status);
}

int test1(void)
{
    volatile unsigned long long v_1 = 0x1ULL;
    volatile unsigned long long v_15 = 0x15ULL;
    volatile unsigned long long v_1ffffffffffff = 0x1ffffffffffffULL;
    volatile unsigned long long v_20 = 0x20ULL;
    volatile unsigned long long v_2000000 = 0x2000000ULL;
    volatile unsigned long long v_2e = 0x2eULL;
    volatile unsigned long long v_38 = 0x38ULL;
    volatile unsigned long long v_3d = 0x3dULL;
    volatile unsigned long long v_3ffffffffffffff = 0x3ffffffffffffffULL;
    volatile unsigned long long v_400000000000000 = 0x400000000000000ULL;
    volatile unsigned long long v_7f = 0x7fULL;
    volatile unsigned long long v_b = 0xbULL;
    volatile unsigned long long v_ffffff = 0xffffffULL;
    unsigned long long expr = (unsigned long long)(((unsigned long long)(((unsigned long long)( - ((unsigned long long)(((unsigned long long)(((unsigned long long)(0x1ULL * (v_3d))) % ((unsigned long long)(0x8000000000000000ULL - 0x6ULL)))) % ((unsigned long long)(((v_3d) < (v_3ffffffffffffff) ? 0x40000000000ULL : 0x10ULL) + ((unsigned long long)(0x1fffffffffULL << (v_2e))))))))) % ((unsigned long long)(((unsigned long long)(((unsigned long long)(0x24ULL - 0x2eULL)) + ((unsigned long long)((v_15) >> 0x1ULL)))) - ((unsigned long long)((0xbULL != (v_400000000000000) ? 0x1aULL : 0x1fULL) << 0x3dULL)))))) - ((unsigned long long)( ~ ((unsigned long long)(((unsigned long long)(((unsigned long long)(((unsigned long long)((v_7f) + 0x2000000000ULL)) % ((unsigned long long)( - 0x7fffffULL)))) / ((unsigned long long)(((unsigned long long)((v_20) / 0x15ULL)) ^ ((unsigned long long)(0x100ULL << (v_3d))))))) | ((unsigned long long)( ~ ((unsigned long long)(((unsigned long long)( ~ ((unsigned long long)(((unsigned long long)( ~ 0x40000ULL)) + ((v_1) > (v_ffffff) ? 0x1fULL : 0x8000000000000000ULL))))) - (((unsigned long long)((v_1ffffffffffff) ^ 0x1ULL)) > ((unsigned long long)((v_2000000) % (v_2e))) ? ((unsigned long long)(((unsigned long long)(0x1ULL >> (v_3d))) >> (v_b))) : ((unsigned long long)(((unsigned long long)(0x24ULL | 0x1aULL)) / ((unsigned long long)((v_38) % 0x1aULL))))))))))))));
    return (expr != 0xbffffffffffbffc3ULL);
}

int main(void)
{
    if (test1() != 0) exit_evil(1);

    exit_ok();
    return 0;
}
