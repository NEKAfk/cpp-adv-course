#pragma once

#include "pack-traits.h"

#include <type_traits>

template <typename... Types>
concept variant_concept = sizeof...(Types) > 0 &&
                          (!(std::is_reference_v<Types> || std::is_array_v<Types> || std::is_same_v<Types, void>) && ...
                          );

template <typename... Types>
  requires variant_concept<Types...>
class variant;

class monostate {};

constexpr bool operator==(monostate, monostate) noexcept {
  return true;
}

constexpr std::strong_ordering operator<=>(monostate, monostate) noexcept {
  return std::strong_ordering::equal;
}

template <size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

template <typename T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <typename T>
inline constexpr in_place_type_t<T> in_place_type{};

class bad_variant_access final : public std::exception {
  const char* what() const noexcept override {
    return "bad_variant_access";
  }
};

template <size_t I, typename T>
struct variant_alternative;

template <size_t I, typename T>
using variant_alternative_t = typename variant_alternative<I, T>::type;

template <typename Head, typename... Tail>
struct variant_alternative<0, variant<Head, Tail...>> {
  using type = Head;
};

template <size_t I, typename Head, typename... Tail>
struct variant_alternative<I, variant<Head, Tail...>> {
  using type = variant_alternative_t<I - 1, variant<Tail...>>;
};

template <size_t I, typename T>
struct variant_alternative<I, const T> {
  using type = std::add_const_t<typename variant_alternative<I, T>::type>;
};

template <typename T>
struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size<const T> : std::integral_constant<size_t, variant_size<T>::value> {};

template <typename T>
inline constexpr size_t variant_size_v = variant_size<T>::value;

inline constexpr size_t variant_npos = static_cast<size_t>(-1);
