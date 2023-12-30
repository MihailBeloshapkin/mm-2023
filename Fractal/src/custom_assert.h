#ifndef _CUSTOM_ASSERT_H_
#define _CUSTOM_ASSERT_H_

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#define _custom_assert(_cond, x, y)                                            \
  do {                                                                         \
    if (!(_cond)) {                                                            \
      printf("custom_assert(%s) fails\nwhere [%s=0x%" PRIx64 "] "              \
             "[%s=0x%" PRIx64 "]\nat %s:%d\n",                                 \
             #_cond, #x, (uint64_t)(uintptr_t)(x), #y,                         \
             (uint64_t)(uintptr_t)(y), __FILE__, __LINE__);                    \
      exit(1);                                                      \
    }                                                                          \
  } while (0)

#define assert_equal(x, y) _custom_assert((x) == (y), x, y)
#define assert_nequal(x, y) _custom_assert((x) != (y), x, y)
#define assert_lt(x, y) _custom_assert((x) < (y), x, y)
#define assert_ge(x, y) _custom_assert((x) >= (y), x, y)

#endif // _CUSTOM_ASSERT_H_
