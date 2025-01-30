#pragma once

#include <cstddef>
#include <type_traits>

template <typename... Types>
class tuple;

namespace detail {
template <size_t N, typename... Types>
struct get_at;

template <size_t N, typename... Types>
using get_at_t = typename get_at<N, Types...>::type;

template <typename Head, typename... Tail>
struct get_at<0, Head, Tail...> {
  using type = Head;
};

template <size_t N, typename Head, typename... Tail>
struct get_at<N, Head, Tail...> {
  using type = get_at_t<N - 1, Tail...>;
};

template <typename Specialization, template <typename...> typename Class>
struct is_specialization_of : std::false_type {};

template <template <typename...> typename Class, typename... T>
struct is_specialization_of<Class<T...>, Class> : std::true_type {};

template <typename Specialization, template <typename...> typename Class>
inline constexpr bool is_specialization_of_v = is_specialization_of<Specialization, Class>::value;

template <typename T>
struct deduce_vtype {
  using type = T;
};

template <typename T>
struct deduce_vtype<std::reference_wrapper<T>> {
  using type = T&;
};

template <typename T>
using deduce_vtype_t = typename deduce_vtype<std::decay_t<T>>::type;

template <typename T, typename... Types>
struct count_type;

template <typename T>
struct count_type<T> {
  static constexpr size_t value = 0;
};

template <typename T, typename... Tail>
struct count_type<T, T, Tail...> {
  static constexpr size_t value = 1 + count_type<T, Tail...>::value;
};

template <typename T, typename Head, typename... Tail>
struct count_type<T, Head, Tail...> {
  static constexpr size_t value = count_type<T, Tail...>::value;
};

template <typename T, typename... Types>
inline constexpr size_t count_type_v = count_type<T, Types...>::value;

template <typename T, typename... Types>
inline constexpr bool is_unique_v = count_type_v<T, Types...> == 1;

template <typename T, typename... Types>
struct get_idx;

template <typename T, typename... Tail>
struct get_idx<T, T, Tail...> {
  static constexpr size_t value = 0;
};

template <typename T, typename Head, typename... Tail>
struct get_idx<T, Head, Tail...> {
  static constexpr size_t value = 1 + get_idx<T, Tail...>::value;
};

template <typename T, typename... Types>
inline constexpr size_t get_idx_v = get_idx<T, Types...>::value;

template <typename... Lists>
struct merge;

template <typename... Lists>
using merge_t = typename merge<Lists...>::type;

template <typename... T>
struct merge<tuple<T...>> {
  using type = tuple<T...>;
};

template <typename... T, typename... S, typename... Tail>
struct merge<tuple<T...>, tuple<S...>, Tail...> {
  using type = merge_t<tuple<T..., S...>, Tail...>;
};

template <typename T>
concept is_copy_list_constructible = requires () { T{}; };

} // namespace detail
