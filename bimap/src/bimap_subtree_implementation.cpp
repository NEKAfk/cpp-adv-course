#include "bimap_subtree_implementation.h"

namespace bimap_subtree_implementation {
void swap_sentinels(base_set_element& lhs, base_set_element& rhs) noexcept {
  std::swap(lhs.parent, rhs.parent);
  std::swap(lhs.left, rhs.left);
  std::swap(lhs.right, rhs.right);
  if (lhs.parent == &rhs) {
    lhs.link_to_this();
  }
  if (rhs.parent == &lhs) {
    rhs.link_to_this();
  }
  lhs.parent->parent = &lhs;
  rhs.parent->parent = &rhs;
}

base_set_element* next(base_set_element* elem) noexcept {
  if (elem->right) {
    elem = elem->right;
    while (elem->left) {
      elem = elem->left;
    }
  } else {
    base_set_element* p = elem->parent;
    while (p->right == elem && elem->parent->parent != elem) {
      elem = p;
      p = elem->parent;
    }
    elem = p;
  }
  return elem;
}

base_set_element* prev(base_set_element* elem) noexcept {
  if (elem->left) {
    elem = elem->left;
    while (elem->right) {
      elem = elem->right;
    }
  } else {
    base_set_element* p = elem->parent;
    while (p->left == elem && elem->parent->parent != elem) {
      elem = p;
      p = elem->parent;
    }
    elem = p;
  }
  return elem;
}

void base_set_element::link_to_this() noexcept {
  left = right = parent = this;
}
} // namespace bimap_subtree_implementation
