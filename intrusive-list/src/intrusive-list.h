#pragma once

#include <algorithm>
#include <cstddef>
#include <type_traits>

namespace intrusive {

class default_tag;

template <typename T>
class list_element;

class base_list_element {
public:
  template <typename T, typename Tag>
    requires std::is_base_of_v<list_element<Tag>, T>
  friend class list;
  base_list_element() = default;

  base_list_element(base_list_element&& other) noexcept;

  base_list_element& operator=(base_list_element&& other) noexcept;

  base_list_element(const base_list_element&) noexcept;

  base_list_element& operator=(const base_list_element& other);

  ~base_list_element();

private:
  bool is_linked() const noexcept;

  void link_on_this() noexcept;

  friend void link(base_list_element* left_node, base_list_element* right_node) noexcept;

  void unlink() noexcept;

  base_list_element* next{this};
  base_list_element* prev{this};
};

template <typename Tag = default_tag>
class list_element : private base_list_element {
  template <typename T, typename ListTag>
    requires std::is_base_of_v<list_element<ListTag>, T>
  friend class list;
};

template <typename T, typename Tag = default_tag>
  requires std::is_base_of_v<list_element<Tag>, T>
class list {
public:
  using node = list_element<Tag>;

private:
  static node& as_node(T& elem) noexcept {
    return static_cast<node&>(elem);
  }

  static node* as_node_ptr(const base_list_element* elem) noexcept {
    return const_cast<node*>(static_cast<const node*>(elem));
  }

  template <class S>
  struct basic_iterator {
    friend class list;

    using value_type = T;
    using difference_type = ptrdiff_t;
    using reference = S&;
    using pointer = S*;
    using iterator_category = std::bidirectional_iterator_tag;

  private:
    explicit basic_iterator(node* current)
        : current_(current) {}

  public:
    basic_iterator() = default;
    basic_iterator(const basic_iterator& other) = default;
    ~basic_iterator() = default;
    basic_iterator& operator=(const basic_iterator& other) = default;

    // Value access

    reference operator*() const noexcept {
      return *static_cast<T*>(current_);
    }

    pointer operator->() const noexcept {
      return static_cast<T*>(current_);
    }

    // Iterator arithmetic operations

    basic_iterator& operator++() & noexcept {
      current_ = as_node_ptr(current_->next);
      return *this;
    }

    basic_iterator operator++(int) & noexcept {
      basic_iterator tmp = *this;
      ++*this;
      return tmp;
    }

    basic_iterator& operator--() & noexcept {
      current_ = as_node_ptr(current_->prev);
      return *this;
    }

    basic_iterator operator--(int) & noexcept {
      basic_iterator tmp = *this;
      --*this;
      return tmp;
    }

    // Comparsion

    friend bool operator==(const basic_iterator& lhs, const basic_iterator& rhs) noexcept {
      return lhs.current_ == rhs.current_;
    }

    friend bool operator!=(const basic_iterator& lhs, const basic_iterator& rhs) noexcept {
      return !(lhs == rhs);
    }

    operator basic_iterator<const S>() const noexcept {
      return {basic_iterator<const S>(current_)};
    }

  private:
    node* current_;
  };

public:
  using iterator = basic_iterator<T>;
  using const_iterator = basic_iterator<const T>;

  // O(1)
  list() noexcept = default;

  // O(1)
  ~list() = default;

  list(const list&) = delete;
  list& operator=(const list&) = delete;

  // O(1)
  list(list&& other) noexcept = default;

  // O(1)
  list& operator=(list&& other) noexcept = default;

  // O(1)
  bool empty() const noexcept {
    return !head_.is_linked();
  }

  // O(n)
  size_t size() const noexcept {
    return std::distance(begin(), end());
  }

  // O(1)
  T& front() noexcept {
    return *begin();
  }

  // O(1)
  const T& front() const noexcept {
    return *begin();
  }

  // O(1)
  T& back() noexcept {
    iterator it = end();
    return *std::prev(it);
  }

  // O(1)
  const T& back() const noexcept {
    const_iterator it = end();
    return *std::prev(it);
  }

  // O(1)
  void push_front(T& value) noexcept {
    insert(begin(), value);
  }

  // O(1)
  void push_back(T& value) noexcept {
    insert(end(), value);
  }

  // O(1)
  void pop_front() noexcept {
    erase(begin());
  }

  // O(1)
  void pop_back() noexcept {
    iterator it = end();
    erase(--it);
  }

  // O(1)
  void clear() noexcept {
    head_.unlink();
  }

  // O(1)
  iterator begin() noexcept {
    return iterator(as_node_ptr(head_.next));
  }

  // O(1)
  const_iterator begin() const noexcept {
    return iterator(as_node_ptr(head_.next));
  }

  // O(1)
  iterator end() noexcept {
    return iterator(&head_);
  }

  // O(1)
  const_iterator end() const noexcept {
    return iterator(const_cast<node*>(&head_));
  }

  // O(1)
  iterator insert(const_iterator pos, T& value) noexcept {
    if (pos.current_ == &as_node(value)) {
      return iterator(pos.current_);
    }
    as_node(value).unlink();
    base_list_element* cur = pos.current_;
    link(cur->prev, &as_node(value));
    link(&as_node(value), cur);
    return iterator(&as_node(value));
  }

  // O(1)
  iterator erase(const_iterator pos) noexcept {
    iterator result = iterator(as_node_ptr(pos.current_->next));
    pos.current_->unlink();
    return result;
  }

  // O(1)
  void splice(const_iterator pos, [[maybe_unused]] list& other, const_iterator first, const_iterator last) noexcept {
    if (first == last) {
      return;
    }
    base_list_element* tmp = first.current_->prev;
    link(pos.current_->prev, first.current_);
    link(last.current_->prev, pos.current_);
    link(tmp, last.current_);
  }

private:
  node head_;
};

} // namespace intrusive
