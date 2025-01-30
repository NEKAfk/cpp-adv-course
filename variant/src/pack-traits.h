#pragma once

#include <cstddef>

namespace detail {
template <typename... Types>
struct TypeList {};

template <typename List1, typename List2>
struct merge;

template <typename... T, typename... S>
struct merge<TypeList<T...>, TypeList<S...>> {
  using type = TypeList<T..., S...>;
};

template <typename List1, typename List2>
using merge_t = typename merge<List1, List2>::type;

template <typename List, size_t N>
struct get_first_types;

template <typename List, size_t N>
using get_first_types_t = typename get_first_types<List, N>::type;

template <typename... Types>
struct get_first_types<TypeList<Types...>, 0> {
  using type = TypeList<>;
};

template <typename Head, typename... Tail, size_t N>
  requires (N >= 1)
struct get_first_types<TypeList<Head, Tail...>, N> {
  using type = merge_t<TypeList<Head>, get_first_types_t<TypeList<Tail...>, N - 1>>;
};

template <typename List, size_t N>
struct get_rest_types;

template <typename List, size_t N>
using get_rest_types_t = typename get_rest_types<List, N>::type;

template <typename... Types>
struct get_rest_types<TypeList<Types...>, 0> {
  using type = TypeList<Types...>;
};

template <typename Head, typename... Tail, size_t N>
  requires (N >= 1)
struct get_rest_types<TypeList<Head, Tail...>, N> {
  using type = get_rest_types_t<TypeList<Tail...>, N - 1>;
};

template <typename T, typename... Types>
struct count_type;

template <typename T, typename... Types>
inline constexpr size_t count_type_v = count_type<T, Types...>::value;

template <typename T>
struct count_type<T> : std::integral_constant<size_t, 0> {};

template <typename T, typename... Tail>
struct count_type<T, T, Tail...> : std::integral_constant<size_t, 1 + count_type_v<T, Tail...>> {};

template <typename T, typename Head, typename... Tail>
struct count_type<T, Head, Tail...> : std::integral_constant<size_t, count_type_v<T, Tail...>> {};

template <typename T, typename... Types>
inline constexpr bool is_unique_v = count_type_v<T, Types...> == 1;

template <typename T, typename... Types>
struct get_idx;

template <typename T, typename... Types>
inline constexpr size_t get_idx_v = get_idx<T, Types...>::value;

template <typename T, typename... Tail>
struct get_idx<T, T, Tail...> : std::integral_constant<size_t, 0> {};

template <typename T, typename Head, typename... Tail>
struct get_idx<T, Head, Tail...> : std::integral_constant<size_t, 1 + get_idx_v<T, Tail...>> {};
} // namespace detail
