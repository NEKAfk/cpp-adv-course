#pragma once

#include <functional>

template <typename Left, typename Right, typename CompareLeft, typename CompareRight>
class bimap;

namespace bimap_subtree_implementation {

struct left_key;
struct right_key;

template <typename Tag, typename L, typename R, typename Comparator>
class set;

struct base_set_element {
  template <typename Tag, typename L, typename R, typename Comparator>
  friend class set;

  template <typename Left, typename Right, typename CompareLeft, typename CompareRight>
  friend class ::bimap;

public:
  base_set_element() = default;

  friend void swap_sentinels(base_set_element& lhs, base_set_element& rhs) noexcept;

  friend base_set_element* next(base_set_element* elem) noexcept;

  friend base_set_element* prev(base_set_element* elem) noexcept;

  ~base_set_element() = default;

private:
  void link_to_this() noexcept;

  base_set_element* left{};
  base_set_element* right{};
  base_set_element* parent{};
};

template <typename T, typename Tag>
struct set_element : base_set_element {};

template <typename Left, typename Right>
struct base_bimap_node
    : set_element<Left, left_key>
    , set_element<Right, right_key> {};

template <typename L, typename R>
struct bimap_node final : base_bimap_node<L, R> {
  template <typename SetTag, typename SetL, typename SetR, typename Comparator>
  friend class set;

  template <typename Left, typename Right, typename CompareLeft, typename CompareRight>
  friend class ::bimap;

  template <typename Left, typename Right>
  bimap_node(Left&& left, Right&& right)
      : left{std::forward<Left>(left)}
      , right{std::forward<Right>(right)} {}

  template <typename Tag>
  const auto& get() const {
    if constexpr (std::is_same_v<Tag, left_key>) {
      return left;
    } else {
      return right;
    }
  }

  L left;
  R right;
};

template <
    typename Tag,
    typename L,
    typename R,
    typename Comparator = std::less<std::conditional_t<std::is_same_v<Tag, left_key>, L, R>>>
class set {
  using T = std::conditional_t<std::is_same_v<Tag, left_key>, L, R>;
  template <typename Left, typename Right, typename CompareLeft, typename CompareRight>
  friend class ::bimap;

public:
  explicit set(Comparator&& comparator)
      : _comparator(std::move(comparator)) {}

  ~set() = default;

  base_set_element* insert(base_set_element* elem, base_set_element* ins_point) const noexcept {
    if (_sentinel == _sentinel->parent) {
      _sentinel->parent = elem;
      elem->parent = _sentinel;
    } else {
      if (ins_point == _sentinel) {
        ins_point->left->right = elem;
        elem->parent = ins_point->left;
      } else {
        ins_point->left = merge(ins_point->left, elem);
        ins_point->left->parent = ins_point;
      }
    }
    if (ins_point == end()) {
      _sentinel->left = elem;
    }
    if (ins_point == begin()) {
      _sentinel->right = elem;
    }
    return elem;
  }

  base_set_element* erase(base_set_element* elem) const noexcept {
    if (_sentinel->left == elem) {
      _sentinel->left = prev(elem);
    }
    auto next_elem = next(elem);
    if (_sentinel->right == elem) {
      _sentinel->right = next_elem;
    }
    auto new_elem = merge(elem->left, elem->right);
    if (new_elem) {
      new_elem->parent = elem->parent;
    }
    if (elem->parent == _sentinel) {
      _sentinel->parent = new_elem ? new_elem : _sentinel;
    } else if (elem->parent->left == elem) {
      elem->parent->left = new_elem;
    } else {
      elem->parent->right = new_elem;
    }
    return next_elem;
  }

  base_set_element* find(const T& val) const {
    auto it = lower_bound(val);
    if (it != end() && equal(as_bimap_node_ptr(it)->template get<Tag>(), val)) {
      return it;
    }
    return end();
  }

  base_set_element* lower_bound(const T& val) const {
    auto f = [this](const T& lhs, const T& rhs) {
      return _comparator(lhs, rhs) || equal(lhs, rhs);
    };
    return conditional_bound(val, f);
  }

  base_set_element* upper_bound(const T& val) const {
    auto f = [this](const T& lhs, const T& rhs) {
      return _comparator(lhs, rhs);
    };
    return conditional_bound(val, f);
  }

  base_set_element* begin() const noexcept {
    return _sentinel->right;
  }

  base_set_element* end() const noexcept {
    return _sentinel;
  }

private:
  void set_sentinel(base_set_element* elem) noexcept {
    _sentinel = elem;
    _sentinel->link_to_this();
  }

  bool equal(const T& lhs, const T& rhs) const {
    return !_comparator(lhs, rhs) && !_comparator(rhs, lhs);
  }

  template <typename Cond>
  base_set_element* conditional_bound(const T& val, Cond cond) const {
    auto result = _sentinel;
    auto tmp = _sentinel->parent;
    if (tmp == result) {
      return result;
    }
    while (tmp) {
      if (cond(val, as_bimap_node_ptr(tmp)->template get<Tag>())) {
        result = tmp;
        tmp = tmp->left;
      } else {
        tmp = tmp->right;
      }
    }
    return result;
  }

  static base_set_element* merge(base_set_element* lhs, base_set_element* rhs) noexcept {
    if (!lhs) {
      return rhs;
    }
    if (!rhs) {
      return lhs;
    }
    rhs->left = merge(lhs, rhs->left);
    if (rhs->left) {
      rhs->left->parent = rhs;
    }
    return rhs;
  }

  static set_element<T, Tag>* as_set_element_ptr(base_set_element* elem) noexcept {
    return static_cast<set_element<T, Tag>*>(elem);
  }

  static bimap_node<L, R>* as_bimap_node_ptr(base_set_element* elem) noexcept {
    return static_cast<bimap_node<L, R>*>(static_cast<base_bimap_node<L, R>*>(static_cast<set_element<T, Tag>*>(elem)));
  }

  base_set_element* _sentinel{};
  [[no_unique_address]] Comparator _comparator;
};
} // namespace bimap_subtree_implementation
