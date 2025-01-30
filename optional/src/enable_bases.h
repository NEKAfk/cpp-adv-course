#pragma once
#include <type_traits>

namespace detail {
template <typename T, bool enable = std::is_copy_constructible_v<T>>
struct enable_copy {
  constexpr enable_copy() = default;

  constexpr enable_copy(const enable_copy&) = delete;

  constexpr enable_copy(enable_copy&&) = default;

  constexpr enable_copy& operator=(const enable_copy&) = default;

  constexpr enable_copy& operator=(enable_copy&&) = default;
};

template <typename T>
struct enable_copy<T, true> {
  // enable_copy(const enable_copy&) = default;
};

template <typename T, bool enable = std::is_move_constructible_v<T>>
struct enable_move {
  constexpr enable_move() = default;

  constexpr enable_move(const enable_move&) = default;

  constexpr enable_move(enable_move&&) = delete;

  constexpr enable_move& operator=(const enable_move&) = default;

  constexpr enable_move& operator=(enable_move&&) = default;
};

template <typename T>
struct enable_move<T, true> {
  // enable_move(const enable_move&&) = default;
};

template <typename T, bool enable = std::is_copy_assignable_v<T> && std::is_copy_constructible_v<T>>
struct enable_copy_assignment {
  constexpr enable_copy_assignment() = default;

  constexpr enable_copy_assignment(const enable_copy_assignment&) = default;

  constexpr enable_copy_assignment(enable_copy_assignment&&) = default;

  constexpr enable_copy_assignment& operator=(const enable_copy_assignment&) = delete;

  constexpr enable_copy_assignment& operator=(enable_copy_assignment&&) = default;
};

template <typename T>
struct enable_copy_assignment<T, true> {
  // enable_copy_assignment& operator=(const enable_copy_assignment&) = default;
};

template <typename T, bool enable = std::is_move_assignable_v<T> && std::is_move_constructible_v<T>>
struct enable_move_assignment {
  constexpr enable_move_assignment() = default;

  constexpr enable_move_assignment(const enable_move_assignment&) = default;

  constexpr enable_move_assignment(enable_move_assignment&&) = default;

  constexpr enable_move_assignment& operator=(const enable_move_assignment&) = default;

  constexpr enable_move_assignment& operator=(enable_move_assignment&&) = delete;
};

template <typename T>
struct enable_move_assignment<T, true> {
  // enable_move_assignment& operator=(enable_move_assignment&&) = default;
};
} // namespace detail
