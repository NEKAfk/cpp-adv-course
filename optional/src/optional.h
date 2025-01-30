#pragma once

#include "copy_assignment_bases.h"
#include "dtor_base.h"
#include "enable_bases.h"

#include <compare>
#include <format>
#include <memory>

struct nullopt_t {
  constexpr explicit nullopt_t(int) {}
};

struct in_place_t {};

inline constexpr in_place_t in_place;
inline constexpr nullopt_t nullopt{4};

namespace detail {
template <typename Specialization, template <typename> typename Class>
struct is_specialization_of : std::false_type {};

template <template <typename> typename Class, typename T>
struct is_specialization_of<Class<T>, Class> : std::true_type {};

template <typename Specialization, template <typename> typename Class>
inline constexpr bool is_specialization_of_v = is_specialization_of<Specialization, Class>::value;

template <typename T>
class optional_base : public dtor_base<T> {
public:
  using dtor_base<T>::dtor_base;

  constexpr optional_base() = default;

  constexpr optional_base(const optional_base&) = default;

  constexpr optional_base(optional_base&&) = default;

  constexpr optional_base& operator=(const optional_base&) = default;

  constexpr optional_base& operator=(optional_base&&) = default;

  constexpr ~optional_base() = default;

  constexpr T& operator*() & noexcept {
    return this->obj;
  }

  constexpr T&& operator*() && noexcept {
    return std::move(this->obj);
  }

  constexpr const T& operator*() const& noexcept {
    return this->obj;
  }

  constexpr const T&& operator*() const&& noexcept {
    return std::move(this->obj);
  }

  constexpr T* operator->() noexcept {
    return std::addressof(this->obj);
  }

  constexpr const T* operator->() const noexcept {
    return std::addressof(this->obj);
  }

  constexpr T& value() {
    if (!has_value()) {
      throw std::runtime_error("Empty");
    }
    return this->obj;
  }

  constexpr const T& value() const {
    if (!has_value()) {
      throw std::runtime_error("Empty");
    }
    return this->obj;
  }

  constexpr bool has_value() const noexcept {
    return this->is_active;
  }

  constexpr explicit operator bool() const noexcept {
    return has_value();
  }
};

} // namespace detail

template <typename T>
class optional
    : detail::enable_copy<T>
    , detail::enable_move<T>
    , detail::enable_copy_assignment<T>
    , detail::enable_move_assignment<T>
    , detail::move_assignment_base<T> {
public:
  using value_type = T;
  using detail::move_assignment_base<T>::move_assignment_base;
  using detail::dtor_base<T>::reset;
  using detail::optional_base<T>::has_value;
  using detail::optional_base<T>::operator*;
  using detail::optional_base<T>::operator->;
  using detail::optional_base<T>::value;
  using detail::optional_base<T>::operator bool;

  constexpr optional() = default;

  constexpr optional(nullopt_t) noexcept
      : optional() {}

  template <
      typename U = T,
      std::enable_if_t<
          (!std::is_same_v<optional, std::remove_cvref_t<U>>) &&
              (!std::is_same_v<in_place_t, std::remove_cvref_t<U>>) && std::is_constructible_v<T, U&&> &&
              !(std::is_same_v<std::remove_cv_t<T>, bool> &&
                detail::is_specialization_of_v<std::remove_cvref_t<U>, optional>),
          bool> = true>
  constexpr explicit(!std::is_convertible_v<U&&, T>)
      optional(U&& value) noexcept(std::is_nothrow_constructible_v<T, U&&>) {
    this->is_active = true;
    std::construct_at(std::addressof(this->obj), std::forward<U>(value));
  }

  template <typename... Args, std::enable_if_t<std::is_constructible_v<T, Args&&...>, bool> = true>
  constexpr explicit optional(in_place_t, Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
    this->is_active = true;
    std::construct_at(std::addressof(this->obj), std::forward<Args>(args)...);
  }

  constexpr optional(const optional&) = default;

  constexpr optional(optional&&) = default;

  constexpr optional& operator=(const optional&) = default;

  constexpr optional& operator=(optional&&) = default;

  constexpr optional& operator=(nullopt_t) noexcept {
    this->reset();
    return *this;
  }

  template <typename U = T>
  constexpr std::enable_if_t<
      (!std::is_same_v<optional, std::remove_cvref_t<U>>) &&
          (!(std::is_scalar_v<T> && std::is_same_v<T, std::decay_t<U>>) ) && std::is_constructible_v<T, U&&> &&
          std::is_assignable_v<T&, U&&>,
      optional&>
  operator=(U&& value) noexcept(std::is_nothrow_constructible_v<T, U&&> && std::is_nothrow_assignable_v<T&, U&&>) {
    if (this->has_value()) {
      **this = std::forward<U>(value);
    } else {
      std::construct_at(std::addressof(this->obj), std::forward<U>(value));
      this->is_active = true;
    }
    return *this;
  }

  template <typename... Args>
  constexpr std::enable_if_t<std::is_constructible_v<T, Args&&...>, T&> emplace(Args&&... args
  ) noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
    this->reset();
    std::construct_at(std::addressof(this->obj), std::forward<Args>(args)...);
    this->is_active = true;
    return **this;
  }

  constexpr void swap(optional& other
  ) noexcept(std::is_nothrow_swappable_v<T> && std::is_nothrow_move_constructible_v<T>) {
    if (other.has_value()) {
      if (this->has_value()) {
        using std::swap;
        swap(**this, *other);
      } else {
        std::construct_at(std::addressof(**this), std::move(*other));
        this->is_active = true;
        other.reset();
      }
    } else {
      if (this->has_value()) {
        other.swap(*this);
      }
    }
  }
};

template <typename T>
constexpr std::enable_if_t<!(std::is_move_constructible_v<T> && std::is_swappable_v<T>), void> swap(
    optional<T>& lhs,
    optional<T>& rhs
) noexcept(std::is_nothrow_swappable_v<T> && std::is_nothrow_move_constructible_v<T>) = delete;

template <typename T>
constexpr std::enable_if_t<std::is_move_constructible_v<T> && std::is_swappable_v<T>, void> swap(
    optional<T>& lhs,
    optional<T>& rhs
) noexcept(std::is_nothrow_swappable_v<T> && std::is_nothrow_move_constructible_v<T>) {
  lhs.swap(rhs);
}

template <typename T>
constexpr bool operator==(const optional<T>& lhs, const optional<T>& rhs) {
  return static_cast<bool>(lhs) == static_cast<bool>(rhs) && (!lhs || *lhs == *rhs);
}

template <typename T>
constexpr bool operator!=(const optional<T>& lhs, const optional<T>& rhs) {
  return static_cast<bool>(lhs) != static_cast<bool>(rhs) || (static_cast<bool>(lhs) && *lhs != *rhs);
}

template <typename T>
constexpr bool operator<(const optional<T>& lhs, const optional<T>& rhs) {
  return static_cast<bool>(rhs) && (!lhs || *lhs < *rhs);
}

template <typename T>
constexpr bool operator<=(const optional<T>& lhs, const optional<T>& rhs) {
  return !lhs || (static_cast<bool>(rhs) && *lhs <= *rhs);
}

template <typename T>
constexpr bool operator>(const optional<T>& lhs, const optional<T>& rhs) {
  return static_cast<bool>(lhs) && (!rhs || *lhs > *rhs);
}

template <typename T>
constexpr bool operator>=(const optional<T>& lhs, const optional<T>& rhs) {
  return !rhs || (static_cast<bool>(lhs) && *lhs >= *rhs);
}

template <class T>
constexpr std::compare_three_way_result_t<T> operator<=>(const optional<T>& lhs, const optional<T>& rhs) {
  return lhs && rhs ? *lhs <=> *rhs : static_cast<bool>(lhs) <=> static_cast<bool>(rhs);
}

template <typename T>
optional(T) -> optional<T>;
