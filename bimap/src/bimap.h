#pragma once

#include "bimap_subtree_implementation.h"

#include <cstddef>
#include <stdexcept>
#include <utility>

template <
    typename Left,
    typename Right,
    typename CompareLeft = std::less<Left>,
    typename CompareRight = std::less<Right>>
class bimap
    : bimap_subtree_implementation::set<bimap_subtree_implementation::left_key, Left, Right, CompareLeft>
    , bimap_subtree_implementation::set<bimap_subtree_implementation::right_key, Left, Right, CompareRight> {
private:
  template <typename Type>
  using type = std::conditional_t<std::is_same_v<Type, bimap_subtree_implementation::left_key>, Left, Right>;

  using left_tree = bimap_subtree_implementation::set<bimap_subtree_implementation::left_key, Left, Right, CompareLeft>;
  using right_tree =
      bimap_subtree_implementation::set<bimap_subtree_implementation::right_key, Left, Right, CompareRight>;

  template <typename Type>
  using tree = std::conditional_t<std::is_same_v<Type, bimap_subtree_implementation::left_key>, left_tree, right_tree>;

  using left_node = bimap_subtree_implementation::set_element<Left, bimap_subtree_implementation::left_key>;
  using right_node = bimap_subtree_implementation::set_element<Right, bimap_subtree_implementation::right_key>;

  template <typename Type>
  using node = std::conditional_t<std::is_same_v<Type, bimap_subtree_implementation::left_key>, left_node, right_node>;

  using base_bimap_node = bimap_subtree_implementation::base_bimap_node<Left, Right>;
  using bimap_node = bimap_subtree_implementation::bimap_node<Left, Right>;

  template <typename T1, typename Tag1, typename T2, typename Tag2>
  class basic_iterator {
    friend class bimap;

  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = T1;
    using difference_type = std::ptrdiff_t;
    using reference = const T1&;
    using pointer = const T1*;

    basic_iterator() = default;
    basic_iterator(const basic_iterator&) = default;
    basic_iterator& operator=(const basic_iterator&) = default;
    ~basic_iterator() = default;

    const T1& operator*() const noexcept {
      return as_bimap_node()->template get<Tag1>();
    }

    const T1* operator->() const noexcept {
      return &as_bimap_node()->template get<Tag1>();
    }

    basic_iterator& operator++() noexcept {
      _current = next(_current);
      return *this;
    }

    basic_iterator operator++(int) noexcept {
      auto tmp = *this;
      ++*this;
      return tmp;
    }

    basic_iterator& operator--() noexcept {
      _current = prev(_current);
      return *this;
    }

    basic_iterator operator--(int) noexcept {
      auto tmp = *this;
      --*this;
      return tmp;
    }

    friend bool operator==(const basic_iterator& lhs, const basic_iterator& rhs) noexcept {
      return lhs._current == rhs._current;
    }

    basic_iterator<T2, Tag2, T1, Tag1> flip() const noexcept {
      return switch_node();
    }

  private:
    using current_node = bimap_subtree_implementation::set_element<T1, Tag1>;
    using connected_node = bimap_subtree_implementation::set_element<T2, Tag2>;

    current_node* as_current_node() const noexcept {
      return static_cast<bimap_subtree_implementation::set_element<T1, Tag1>*>(_current);
    }

    base_bimap_node* as_base_bimap_node() const noexcept {
      return static_cast<base_bimap_node*>(as_current_node());
    }

    bimap_node* as_bimap_node() const noexcept {
      return static_cast<bimap_node*>(as_base_bimap_node());
    }

    connected_node* as_connected_node() const noexcept {
      return static_cast<bimap_subtree_implementation::set_element<T2, Tag2>*>(as_bimap_node());
    }

    bimap_subtree_implementation::base_set_element* switch_node() const noexcept {
      return static_cast<bimap_subtree_implementation::base_set_element*>(as_connected_node());
    }

    basic_iterator(bimap_subtree_implementation::base_set_element* current) noexcept
        : _current{current} {}

    bimap_subtree_implementation::base_set_element* _current;
  };

public:
  using left_t = Left;
  using right_t = Right;

  using node_t = bimap_node;

  using left_iterator =
      basic_iterator<Left, bimap_subtree_implementation::left_key, Right, bimap_subtree_implementation::right_key>;
  using right_iterator =
      basic_iterator<Right, bimap_subtree_implementation::right_key, Left, bimap_subtree_implementation::left_key>;

private:
  template <typename Type>
  using iter =
      std::conditional_t<std::is_same_v<Type, bimap_subtree_implementation::left_key>, left_iterator, right_iterator>;

public:
  bimap(CompareLeft compare_left = CompareLeft(), CompareRight compare_right = CompareRight())
      : left_tree{std::move(compare_left)}
      , right_tree{std::move(compare_right)} {
    left_tree::set_sentinel(as_left_node(&_sentinel));
    right_tree::set_sentinel(as_right_node(&_sentinel));
  }

  bimap(const bimap& other)
    requires (std::is_copy_constructible_v<CompareLeft> && std::is_copy_constructible_v<CompareRight>)
      : bimap(other.left_tree::_comparator, other.right_tree::_comparator) {
    for (auto it = other.begin_left(); it != other.end_left(); ++it) {
      insert(*it, *it.flip());
    }
  }

  bimap(bimap&& other) noexcept
      : bimap(std::move(other.left_tree::_comparator), std::move(other.right_tree::_comparator)) {
    swap_sentinels(*left_tree::_sentinel, *other.left_tree::_sentinel);
    swap_sentinels(*right_tree::_sentinel, *other.right_tree::_sentinel);
    _size = std::exchange(other._size, 0);
  }

  bimap& operator=(const bimap& other) {
    if (this == &other) {
      return *this;
    }
    bimap tmp{other};
    swap(*this, tmp);
    return *this;
  }

  bimap& operator=(bimap&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    bimap tmp{std::move(other)};
    swap(*this, tmp);
    return *this;
  }

  ~bimap() {
    erase_left(begin_left(), end_left());
  }

  friend void swap(bimap& lhs, bimap& rhs) noexcept {
    using std::swap;
    swap(lhs._size, rhs._size);
    swap(static_cast<left_tree&>(lhs)._comparator, static_cast<left_tree&>(rhs)._comparator);
    swap(static_cast<right_tree&>(lhs)._comparator, static_cast<right_tree&>(rhs)._comparator);
    swap_sentinels(*static_cast<left_tree&>(lhs)._sentinel, *static_cast<left_tree&>(rhs)._sentinel);
    swap_sentinels(*static_cast<right_tree&>(lhs)._sentinel, *static_cast<right_tree&>(rhs)._sentinel);
  }

  left_iterator insert(const left_t& left, const right_t& right) {
    return insert_impl(left, right);
  }

  left_iterator insert(const left_t& left, right_t&& right) {
    return insert_impl(left, std::move(right));
  }

  left_iterator insert(left_t&& left, const right_t& right) {
    return insert_impl(std::move(left), right);
  }

  left_iterator insert(left_t&& left, right_t&& right) {
    return insert_impl(std::move(left), std::move(right));
  }

  left_iterator erase_left(left_iterator it) noexcept {
    --_size;
    right_tree::erase(it.flip()._current);
    auto result = left_tree::erase(it._current);
    delete it.as_bimap_node();
    return result;
  }

  right_iterator erase_right(right_iterator it) noexcept {
    --_size;
    left_tree::erase(it.flip()._current);
    auto result = right_tree::erase(it._current);
    delete it.as_bimap_node();
    return result;
  }

  bool erase_left(const left_t& left) {
    auto it = left_iterator(left_tree::find(left));
    if (it == end_left()) {
      return false;
    }
    erase_left(it);
    return true;
  }

  bool erase_right(const right_t& right) {
    auto it = right_iterator(right_tree::find(right));
    if (it == end_right()) {
      return false;
    }
    erase_right(it);
    return true;
  }

  left_iterator erase_left(left_iterator first, left_iterator last) noexcept {
    while (first != last) {
      first = erase_left(first);
    }
    return last;
  }

  right_iterator erase_right(right_iterator first, right_iterator last) noexcept {
    while (first != last) {
      first = erase_right(first);
    }
    return last;
  }

  left_iterator find_left(const left_t& left) const {
    return left_tree::find(left);
  }

  right_iterator find_right(const right_t& right) const {
    return right_tree::find(right);
  }

  const right_t& at_left(const left_t& key) const {
    auto it = find_left(key);
    if (it == end_left()) {
      throw std::out_of_range("bimap::at_left");
    }
    return *it.flip();
  }

  const left_t& at_right(const right_t& key) const {
    auto it = find_right(key);
    if (it == end_right()) {
      throw std::out_of_range("bimap::at_right");
    }
    return *it.flip();
  }

  const right_t& at_left_or_default(const left_t& key)
    requires std::is_default_constructible_v<right_t>
  {
    return at_or_default_impl<bimap_subtree_implementation::left_key, bimap_subtree_implementation::right_key>(key);
  }

  const left_t& at_right_or_default(const right_t& key)
    requires std::is_default_constructible_v<left_t>
  {
    return at_or_default_impl<bimap_subtree_implementation::right_key, bimap_subtree_implementation::left_key>(key);
  }

  left_iterator lower_bound_left(const left_t& left) const {
    return left_tree::lower_bound(left);
  }

  left_iterator upper_bound_left(const left_t& left) const {
    return left_tree::upper_bound(left);
  }

  right_iterator lower_bound_right(const right_t& right) const {
    return right_tree::lower_bound(right);
  }

  right_iterator upper_bound_right(const right_t& right) const {
    return right_tree::upper_bound(right);
  }

  left_iterator begin_left() const noexcept {
    return left_tree::begin();
  }

  left_iterator end_left() const noexcept {
    return left_tree::end();
  }

  right_iterator begin_right() const noexcept {
    return right_tree::begin();
  }

  right_iterator end_right() const noexcept {
    return right_tree::end();
  }

  bool empty() const noexcept {
    return size() == 0;
  }

  std::size_t size() const noexcept {
    return _size;
  }

  friend bool operator==(const bimap& lhs, const bimap& rhs) {
    if (lhs.size() != rhs.size()) {
      return false;
    }
    auto it_lhs = lhs.begin_left();
    auto it_rhs = rhs.begin_left();
    while (it_lhs != lhs.end_left()) {
      if (!lhs.left_tree::equal(*it_lhs, *it_rhs) || !lhs.right_tree::equal(*it_lhs.flip(), *it_rhs.flip())) {
        return false;
      }
      ++it_lhs;
      ++it_rhs;
    }
    return true;
  }

private:
  template <typename K, typename V>
    requires std::is_default_constructible_v<type<V>>
  const type<V>& at_or_default_impl(const type<K>& key) {
    auto it_key = iter<K>(tree<K>::lower_bound(key));
    if (it_key != iter<K>(tree<K>::end()) && tree<K>::equal(*it_key, key)) {
      return *it_key.flip();
    }
    type<V> value = type<V>();
    auto it_value = iter<V>(tree<V>::find(value));
    if (it_value == iter<V>(tree<V>::end())) {
      if constexpr (std::is_same_v<K, bimap_subtree_implementation::left_key>) {
        auto it = insert(key, std::move(value));
        return *it.flip();
      } else {
        auto it = insert(std::move(value), key);
        return *it;
      }
    } else {
      if (it_value.flip() == it_key) {
        ++it_key;
      }
      bimap_node* elem = nullptr;
      if constexpr (std::is_same_v<K, bimap_subtree_implementation::left_key> &&
                    std::is_same_v<V, bimap_subtree_implementation::right_key>) {
        elem = new bimap_node(key, std::move(value));
        insert_before(elem, it_key, erase_right(it_value));
      } else {
        elem = new bimap_node(std::move(value), key);
        insert_before(elem, erase_left(it_value), it_key);
      }
      ++_size;
      return elem->template get<V>();
    }
  }

  template <typename L, typename R>
  left_iterator insert_impl(L&& left, R&& right) {
    auto it_left = lower_bound_left(left);
    if (it_left != end_left() && left_tree::equal(left, *it_left)) {
      return end_left();
    }
    auto it_right = lower_bound_right(right);
    if (it_right != end_right() && right_tree::equal(right, *it_right)) {
      return end_left();
    }
    auto elem = new bimap_node(std::forward<L>(left), std::forward<R>(right));
    ++_size;
    return insert_before(elem, it_left, it_right);
  }

  left_iterator insert_before(bimap_node* elem, left_iterator it_left, right_iterator it_right) noexcept {
    right_tree::insert(
        static_cast<bimap_subtree_implementation::base_set_element*>(as_right_node(elem)),
        it_right._current
    );
    return left_tree::insert(
        static_cast<bimap_subtree_implementation::base_set_element*>(as_left_node(elem)),
        it_left._current
    );
  }

  template <typename U>
  static left_node* as_left_node(U* node) {
    return static_cast<left_node*>(node);
  }

  template <typename U>
  static right_node* as_right_node(U* node) {
    return static_cast<right_node*>(node);
  }

  base_bimap_node _sentinel;
  size_t _size{0};
};
