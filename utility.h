
typedef long int           s64;
typedef int                s32;
typedef short int          s16;
typedef char               s8;
typedef long unsigned int  u64;
typedef unsigned int       u32;
typedef short unsigned int u16;
typedef unsigned char      u8;
typedef float              f32;
typedef double             f64;


#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

#define cyan(A) ANSI_COLOR_CYAN A ANSI_COLOR_RESET
#define blue(A) ANSI_COLOR_BLUE A ANSI_COLOR_RESET
#define yellow(A) ANSI_COLOR_YELLOW A ANSI_COLOR_RESET
#define green(A) ANSI_COLOR_GREEN A ANSI_COLOR_RESET
#define red(A) ANSI_COLOR_RED A ANSI_COLOR_RESET


template<class T, class U>
struct pair {
  T fst;
  U snd;
};

template<class T, class U>
struct is_same       : std::false_type {};
template<class T>
struct is_same<T, T> : std::true_type {};


struct Junk {};
#define defer auto ANONYMOUS_NAME = Junk{} + [&]()
#define ANONYMOUS_NAME    CONCAT(GAMMA, __LINE__)
#define CONCAT(A, B)      CONCAT_IMPL(A, B)
#define CONCAT_IMPL(A, B) A##B


template<class F>
struct ScopeGuard {
  F f;

  ScopeGuard(F &&f__) : f(std::move(f__)) {}
  ~ScopeGuard() { f(); }
};

template<class F>
ScopeGuard<F> operator+(Junk, F &&fun) {
  return ScopeGuard<F>(std::move(fun));
}



struct literal {
  const char *data = NULL;
  size_t      size = 0;

  literal() = default;
  template<size_t N>
  literal(const char (&x)[N])      : data(x), size(N-1) {}
  literal(const char *x, size_t s) : data(x), size(s)   {}

  literal(const literal &)            = default;
  literal &operator=(const literal &) = default;
  literal(literal &&)                 = default;
  literal &operator=(literal &&)      = default;
};

#define get_string_from_literal(name, l) \
  char name[l.size+1]; \
  memcpy(name, l.data, l.size); \
  name[l.size] = '\0'

inline bool operator==(const char *s, const literal &l) {
  return !strncmp(s, l.data, l.size);
}

inline bool operator==(const literal &l, const char *s) {
  return !strncmp(s, l.data, l.size);
}

inline bool operator==(const literal &l1, const literal &l2) {
  if(l1.size == l2.size) {
    return !strncmp(l1.data, l2.data, l1.size);
  } else {
    return false;
  }
}

inline std::ostream& operator<<(std::ostream &os, const literal &l) {
  for(size_t i = 0; i < l.size; i++) {
    os << l.data[i];
  }
  return os;
}


void inline print() { std::cout << std::endl; }

template<class T, class ...Args>
void print(T&& first, Args&&... rest) {
  std::cout << std::boolalpha << std::forward<T>(first);
  print(std::forward<Args>(rest)...);
}

template<class T, class U, size_t N>
bool is_one_of(const U c, const T (&x)[N], size_t *index=NULL) {
  for(size_t i = 0; i < N; i++) {
    if(c == x[i]) { 
      if(index) *index = i;
      return true; 
    }
  }
  return false;
}


#define array_size(x) (sizeof((x)) / sizeof(*(x)))

inline void INC(const char *&c) {
  assert(*c != '\0');
  ++c;
}

inline bool next_n_symbols_nz(const char *c, size_t n) {
  for(size_t i = 0; i < n; i++) {
    if(*c == '\0') { return false; }
    ++c;
  }
  return true;
}

inline void ADVANCE(const char *&c, size_t n) {
  assert(next_n_symbols_nz(c, n));
  c += n;
}

#define define_functor(name, op) \
  struct name { \
    template<class T, class U> \
    auto operator()(T a, U b) -> decltype(a op b) { \
      return a op b; \
    } \
  }

define_functor(add, +);
define_functor(subtract, -);
define_functor(multiply, *);
define_functor(divide, /);
define_functor(bitwise_and, &);
define_functor(bitwise_or, |);
define_functor(bitwise_xor, ^);
define_functor(equals, ==);
define_functor(not_equals, !=);
define_functor(modulo, %);
define_functor(less, <);
define_functor(greater, >);
define_functor(less_or_equals, <=);
define_functor(greater_or_equals, >=);
define_functor(bit_and, &);
define_functor(bit_or, |);
define_functor(bit_xor, ^);

