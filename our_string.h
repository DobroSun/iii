#ifndef MY_STRING_IMPL_H
#define MY_STRING_IMPL_H


inline void *allocate_and_zero(size_t bits) {
  void *r = malloc(bits);
  assert(r);
  memset(r, 0, bits);
  return r;
}

struct string {
  char *data = NULL;
  s32   capacity = 0;
  s32   size     = 0;


  string() = default;

  template<size_t N>
  string(const char (&x)[N]) {
    data     = (char*)allocate_and_zero(N);
    memcpy(data, x, N);
    capacity = N;
    size     = N-1;
  }

  string(const char *x, size_t N) {
    data     = (char*)allocate_and_zero(N+1);
    memcpy(data, x, N+1);
    capacity = N+1;
    size     = N;
  }

  ~string() {
    free(data);
    new (this) string();
  }


  void add(char c) {
    if(size == capacity) { reserve(); }
    data[size++] = c;
  }

  char &pop() { 
    assert(size > 0);
    return data[--size];
  }

  char &operator[](s32 index) {
    assert(index >= 0 && index < size);
    return data[index];
  }

  const char &operator[](s32 index) const {
    assert(index >= 0 && index < size);
    return data[index];
  }

  void reserve(s32 new_cap=0) {
    if(!data) {
      assert(!capacity && !size && !data);

      new_cap  = (new_cap)? new_cap: 8;
      ++new_cap;                              // null terminator.
      data     = (char*)allocate_and_zero(new_cap);
      capacity = new_cap;

    } else {
      assert(data && capacity);
      new_cap = (new_cap)? new_cap: 2*capacity;
      ++new_cap;                              // null terminator.

      if(new_cap > capacity) {
        auto new_data = (char*)allocate_and_zero(new_cap);

        if(size) { memcpy(new_data, data, size); }
        capacity = new_cap;

        free(data);
        data = new_data;
      } else {
        // There is no point in shrinking down the allocation. (I think).
      }
    }
  }

  void resize(s32 new_cap=0) {
    reserve(new_cap+1);
    assert(data[capacity-1] == '\0');
    size = capacity-2;
  }

  void  clear()       { size = 0; }
  bool  empty() const { return size == 0; }

        char &first()       { return data[0]; }
  const char &first() const { return data[0]; }
        char &last()        { return data[size-1]; }
  const char &last() const  { return data[size-1]; }


  struct iterator {
    s32 index; char *p;
    
    explicit iterator(s32 i)          { index = i; }
    explicit iterator(s32 i, char* d) { index = i; p = d; }
    iterator& operator++()            { ++index; return *this; }
    iterator& operator++(int)         { ++index; return *this; }
    bool operator==(iterator o) const { return index == o.index; }
    bool operator!=(iterator o) const { return index != o.index; }
    char& operator*()           const { return p[index]; }
  };
  iterator begin() const { return iterator(0, data); }
  iterator end()   const { return iterator(size);    }
};

std::ostream &operator<<(std::ostream &os, const string &s) {
  for(const auto &c : s) { os << c; }
  return os;
}

inline bool operator==(const string &a, const string &b) {
  if(a.size == b.size) {
    return !strncmp(a.data, b.data, a.size);
  } else {
    return false;
  }
}
inline bool operator==(const string &b, const char *a) { return !strncmp(a, b.data, b.size); }
inline bool operator==(const char *a, const string &b) { return !strncmp(a, b.data, b.size); }

inline void copy_array(string *a, const string *b) {
  if(a->capacity <= b->size) {
    a->data = (char*)allocate_and_zero(b->size);
    a->capacity = b->size;
  }
  memcpy(a->data, b->data, b->size);
  a->size = b->size;
}

inline void move_array(string *a, string *b) {
  memcpy(a, b, sizeof(string));
  new (b) string();
}

#endif
