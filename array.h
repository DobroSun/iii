#ifndef MY_ARRAY_IMPL_H
#define MY_ARRAY_IMPL_H


template<class T>
struct array {
  T *data      = NULL;
  s32 capacity = 0;
  s32 size     = 0;


  array() = default;
  ~array() {
    delete[] data;
    data     = NULL;
    capacity = 0;
    size     = 0;
  }

  T &add() {
    if(size == capacity) { reserve(); }
    return data[size++];
  }

  T &add(const T &val) {
    return add() = val;
  }

  T &add(T &&val) {
    return add() = std::move(val);
  }

  void find(const T &val, T **iter) {
    for(s32 i = 0; i < size; i++) {
      if(this->operator[](i) == val) {
        *iter = &this->operator[](i);
        return;
      }
    }
    *iter = NULL;
  }

  void find(const T &val, T **iter, s32 *index) { // @Copy&Paste:
    for(s32 i = 0; i < size; i++) {
      if(this->operator[](i) == val) {
        *iter  = &this->operator[](i);
        *index = i;
        return;
      }
    }
    *iter = NULL;
  }



  s32 remove(const T &val) {
    T *iter; s32 index;
    find(&iter, &index, val);

    assert(iter);

    memcpy(data+index, data+index+1, sizeof(T) * (size-index-1));
    size--;

    return index;
  }

  T &insert(s32 index) {
    if(index >= size)    { return add(); }
    if(size == capacity) { reserve(); }

    const s32 size_to_copy = size - index;
    size++;

    T tmp[size_to_copy];

    memcpy(tmp, data + index, sizeof(tmp));
    memcpy(data + index + 1, tmp, sizeof(tmp));

    return data[index];
  }

  T &insert(const T &c, s32 index) {
    return insert(index) = c;
  }

  T &insert(T &&c, s32 index) {
    return insert(index) = std::move(c);
  }

  T &pop()   { return data[--size]; }
  T &first() { return data[0]; }
  T &last()  { return data[size-1]; }

  void reserve(s32 new_cap=0) {
    if(!data) {
      assert(!capacity && !size && !data);

      new_cap  = (new_cap)? new_cap: 8;
      data     = new T[new_cap];
      capacity = new_cap;

    } else {
      assert(data && capacity);
      new_cap = (new_cap)? new_cap: 2*capacity;

      if(new_cap > capacity) {
        auto new_data = new T[new_cap];
        assert(new_data);
        if(size) {
          memcpy(new_data, data, sizeof(T) * size);
        }
        capacity = new_cap;

        delete[] data;
        data = new_data;

      } else {
        assert(new_cap <= capacity);
        if(size > new_cap) {
          size     = new_cap;
          capacity = new_cap;

          auto new_data = new T[new_cap];
          assert(new_data);

          memcpy(new_data, data, sizeof(T) * size);

          delete[] data;
          data = new_data;

        } else {
          // Do nothing.
        }
      }
    }
  }

  void resize(s32 new_size=0) {
    reserve(new_size);
    size = capacity;
  }

  void clear() {
    size = 0;
  }

  bool empty() const {
    return size == 0;
  }

  T &operator[](s32 index) {
    assert(index >= 0 && index < size);
    return data[index];
  }

  const T &operator[](s32 index) const {
    assert(index >= 0 && index < size);
    return data[index];
  }


  struct iterator {
    s32 index; T *p;
    
    explicit iterator(s32 i)          { index = i; }
    explicit iterator(s32 i, T* d)    { index = i; p = d; }
    iterator& operator++()            { ++index; return *this; }
    iterator& operator++(int)         { ++index; return *this; }
    bool operator==(iterator o) const { return index == o.index; }
    bool operator!=(iterator o) const { return index != o.index; }
    T& operator*()              const { return p[index]; }
  };
  iterator begin() { return iterator(0, data); }
  iterator end()   { return iterator(size);    }
};

template<class T>
bool operator==(const array<T> &a, const array<T> &b) {
  if(a.size == b.size) {
    for(s32 i = 0; i < a.size; i++) {
      if(a[i] != b[i]) { return false; }
    }
    return true;
  } else {
    return false;
  }
}

template<class T>
void copy_array(array<T> *a, const array<T> *b) {
  if(a->capacity <= b->size) {
    delete[] a->data;
    a->data = new T[b->size];
    a->capacity = b->size;
  }
  memcpy(a->data, b->data, sizeof(T) * b->size);
  a->size     = b->size;
}

template<class T>
void move_array(array<T> *a, array<T> *b) {
  memcpy(a, b, sizeof(array<T>));
  new (b) array<T>();
}

#endif
