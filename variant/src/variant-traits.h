#pragma once

#include "variant-helper.h"

namespace detail {

// traits
template <typename... Types>
struct traits {
  static constexpr bool is_trivially_copy_constructible_v = (std::is_trivially_copy_constructible_v<Types> && ...);
  static constexpr bool is_trivially_move_constructible_v = (std::is_trivially_move_constructible_v<Types> && ...);
  static constexpr bool is_trivially_copy_assignable_v = (std::is_trivially_copy_assignable_v<Types> && ...);
  static constexpr bool is_trivially_move_assignable_v = (std::is_trivially_move_assignable_v<Types> && ...);
  static constexpr bool is_trivially_destructible_v = (std::is_trivially_destructible_v<Types> && ...);

  static constexpr bool is_copy_constructible_v = (std::is_copy_constructible_v<Types> && ...);
  static constexpr bool is_move_constructible_v = (std::is_move_constructible_v<Types> && ...);
  static constexpr bool is_copy_assignable_v = (std::is_copy_assignable_v<Types> && ...);
  static constexpr bool is_move_assignable_v = (std::is_move_assignable_v<Types> && ...);

  static constexpr bool is_nothrow_copy_constructible_v = (std::is_nothrow_copy_constructible_v<Types> && ...);
  static constexpr bool is_nothrow_move_constructible_v = (std::is_nothrow_move_constructible_v<Types> && ...);
  static constexpr bool is_nothrow_copy_assignable_v = (std::is_nothrow_copy_assignable_v<Types> && ...);
  static constexpr bool is_nothrow_move_assignable_v = (std::is_nothrow_move_assignable_v<Types> && ...);

  static constexpr bool is_swappable_v = (std::is_swappable_v<Types> && ...);

  static constexpr bool is_nothrow_swappable_v = (std::is_nothrow_swappable_v<Types> && ...);
};

// concepts
template <typename... Types>
concept var_copy_ctor = traits<Types...>::is_copy_constructible_v;

template <typename... Types>
concept var_triv_copy_ctor = var_copy_ctor<Types...> && traits<Types...>::is_trivially_copy_constructible_v;

template <typename... Types>
concept var_move_ctor = traits<Types...>::is_move_constructible_v;

template <typename... Types>
concept var_triv_move_ctor = var_move_ctor<Types...> && traits<Types...>::is_trivially_move_constructible_v;

template <typename... Types>
concept var_copy_assignment = traits<Types...>::is_copy_constructible_v && traits<Types...>::is_copy_assignable_v;

template <typename... Types>
concept var_triv_copy_assignment =
    var_copy_assignment<Types...> && traits<Types...>::is_trivially_copy_constructible_v &&
    traits<Types...>::is_trivially_copy_assignable_v && traits<Types...>::is_trivially_destructible_v;

template <typename... Types>
concept var_move_assignment = traits<Types...>::is_move_constructible_v && traits<Types...>::is_move_assignable_v;

template <typename... Types>
concept var_triv_move_assignment =
    var_move_assignment<Types...> && traits<Types...>::is_trivially_move_constructible_v &&
    traits<Types...>::is_trivially_move_assignable_v && traits<Types...>::is_trivially_destructible_v;

// resolve variant alternative to use
template <typename T, typename T_i>
concept array_constructible = requires (T t) { new T_i[1]{std::forward<T>(t)}; };

template <size_t I, typename T, typename T_i>
struct type_resolve_base {
  std::integral_constant<size_t, I> f() = delete;
};

template <size_t I, typename T, typename T_i>
  requires (array_constructible<T, T_i>)
struct type_resolve_base<I, T, T_i> {
  static std::integral_constant<size_t, I> f(T_i);
};

template <typename T, typename V, typename = std::make_index_sequence<variant_size_v<V>>>
struct type_resolve;

template <typename T, typename... Types, size_t... I>
struct type_resolve<T, variant<Types...>, std::index_sequence<I...>> : type_resolve_base<I, T, Types>... {
  using type_resolve_base<I, T, Types>::f...;
};

template <typename T, typename... Types>
inline constexpr size_t type_resolve_idx =
    decltype(type_resolve<T, variant<Types...>>::f(std::forward<T>(std::declval<T>())))::value;

template <typename T, typename... Types>
using type_resolve_t = variant_alternative_t<
    decltype(type_resolve<T, variant<Types...>>::f(std::forward<T>(std::declval<T>())))::value,
    variant<Types...>>;

// check for place-tag
template <typename T>
struct is_in_place_tag : std::false_type {};

template <typename T>
struct is_in_place_tag<in_place_type_t<T>> : std::true_type {};

template <size_t I>
struct is_in_place_tag<in_place_index_t<I>> : std::true_type {};

template <typename T>
inline constexpr bool is_in_place_tag_v = is_in_place_tag<T>::value;

} // namespace detail
