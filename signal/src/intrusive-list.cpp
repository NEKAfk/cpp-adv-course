#include "intrusive-list.h"

namespace intrusive {
base_list_element::base_list_element(base_list_element&& other) noexcept
    : base_list_element() {
  *this = std::move(other);
}

base_list_element& base_list_element::operator=(base_list_element&& other) noexcept {
  if (this != &other) {
    unlink();
    if (other.is_linked()) {
      link(other.prev, this);
      link(this, other.next);
      other.link_on_this();
    } else {
      link_on_this();
    }
  }
  return *this;
}

base_list_element::base_list_element(const base_list_element&) noexcept {}

base_list_element& base_list_element::operator=(const base_list_element& other) {
  if (this != &other) {
    unlink();
  }
  return *this;
}

bool base_list_element::is_linked() const noexcept {
  return next != this;
}

void base_list_element::link_on_this() noexcept {
  next = prev = this;
}

void link(base_list_element* left_node, base_list_element* right_node) noexcept {
  left_node->next = right_node;
  right_node->prev = left_node;
}

void base_list_element::unlink() noexcept {
  link(prev, next);
  link_on_this();
}

base_list_element::~base_list_element() {
  unlink();
}

} // namespace intrusive
