#pragma once
#include <memory>
#include <type_traits>

namespace detail {
template <typename T>
class optional_base;

template <typename T, bool is_trivial = std::is_trivially_copy_constructible_v<T>>
struct copy_base : optional_base<T> {
  using optional_base<T>::optional_base;

  constexpr copy_base() = default;

  constexpr copy_base(copy_base&&) = default;

  constexpr copy_base& operator=(const copy_base&) = default;

  constexpr copy_base& operator=(copy_base&&) = default;

  constexpr copy_base(const copy_base& other) noexcept(std::is_nothrow_copy_constructible_v<T>)
      : optional_base<T>() {
    if (other.has_value()) {
      std::construct_at(&this->obj, *other);
      this->is_active = true;
    }
  }
};

template <typename T>
struct copy_base<T, true> : optional_base<T> {
  using optional_base<T>::optional_base;
};

template <typename T, bool is_trivial = std::is_trivially_move_constructible_v<T>>
struct move_base : copy_base<T> {
  using copy_base<T>::copy_base;

  constexpr move_base() = default;

  constexpr move_base(const move_base&) = default;

  constexpr move_base& operator=(const move_base&) = default;

  constexpr move_base& operator=(move_base&&) = default;

  constexpr move_base(move_base&& other) noexcept(std::is_nothrow_move_constructible_v<T>) {
    if (other.has_value()) {
      std::construct_at(&this->obj, std::move(*other));
      this->is_active = true;
    }
  }
};

template <typename T>
struct move_base<T, true> : copy_base<T> {
  using copy_base<T>::copy_base;
};

template <
    typename T,
    bool is_trivial = std::is_trivially_copy_assignable_v<T> && std::is_trivially_copy_constructible_v<T>>
struct copy_assignment_base : move_base<T> {
  using move_base<T>::move_base;

  constexpr copy_assignment_base() = default;

  constexpr copy_assignment_base(const copy_assignment_base& other) = default;

  constexpr copy_assignment_base(copy_assignment_base&&) = default;

  constexpr copy_assignment_base& operator=(copy_assignment_base&&) = default;

  constexpr copy_assignment_base& operator=(const copy_assignment_base& other
  ) noexcept(std::is_nothrow_copy_assignable_v<T> && std::is_nothrow_copy_constructible_v<T>) {
    if (this != &other) {
      if (other.has_value()) {
        if (this->has_value()) {
          **this = *other;
        } else {
          std::construct_at(std::addressof(this->obj), *other);
          this->is_active = true;
        }
      } else {
        this->reset();
      }
    }
    return *this;
  }
};

template <typename T>
struct copy_assignment_base<T, true> : move_base<T> {
  using move_base<T>::move_base;
};

template <
    typename T,
    bool is_trivial = std::is_trivially_move_assignable_v<T> && std::is_trivially_move_constructible_v<T>>
struct move_assignment_base : copy_assignment_base<T> {
  using copy_assignment_base<T>::copy_assignment_base;

  constexpr move_assignment_base() = default;

  constexpr move_assignment_base(const move_assignment_base& other) = default;

  constexpr move_assignment_base(move_assignment_base&&) = default;

  constexpr move_assignment_base& operator=(const move_assignment_base&) = default;

  constexpr move_assignment_base& operator=(move_assignment_base&& other
  ) noexcept(std::is_nothrow_move_assignable_v<T> && std::is_nothrow_move_constructible_v<T>) {
    if (this != &other) {
      if (other.has_value()) {
        if (this->has_value()) {
          **this = std::move(*other);
        } else {
          std::construct_at(std::addressof(this->obj), std::move(*other));
          this->is_active = true;
        }
      } else {
        this->reset();
      }
    }
    return *this;
  }
};

template <typename T>
struct move_assignment_base<T, true> : copy_assignment_base<T> {
  using copy_assignment_base<T>::copy_assignment_base;
};
} // namespace detail
