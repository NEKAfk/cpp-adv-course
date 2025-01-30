#pragma once

#include "variant-helper.h"

namespace detail {
template <bool is_triv_dtor, typename TypeList>
union var_union {
  constexpr var_union() = default;
};

template <bool is_triv_dtor, typename T>
union var_union<is_triv_dtor, TypeList<T>> {
  constexpr var_union() {}

  constexpr var_union(const var_union&) = default;
  constexpr var_union(var_union&&) = default;
  constexpr var_union& operator=(const var_union&) = default;
  constexpr var_union& operator=(var_union&&) = default;

  template <typename... Args>
  constexpr explicit var_union(in_place_index_t<0>, Args&&... args)
      : cur(std::forward<Args>(args)...) {}

  constexpr const T& get() const& noexcept {
    return cur;
  }

  constexpr T& get() & noexcept {
    return cur;
  }

  constexpr const T&& get() const&& noexcept {
    return std::move(cur);
  }

  constexpr T&& get() && noexcept {
    return std::move(cur);
  }

  constexpr ~var_union() = default;

  constexpr ~var_union()
    requires (!is_triv_dtor)
  {}

  T cur;
};

template <bool is_triv_dtor, typename... Types>
union var_union<is_triv_dtor, TypeList<Types...>> {
  constexpr var_union()
      : fst{} {}

  template <size_t N, typename... Args>
    requires (N < sizeof...(Types) / 2)
  explicit constexpr var_union(in_place_index_t<N>, Args&&... args)
      : fst(in_place_index<N>, std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
    requires (N >= sizeof...(Types) / 2)
  explicit constexpr var_union(in_place_index_t<N>, Args&&... args)
      : snd(in_place_index<N - sizeof...(Types) / 2>, std::forward<Args>(args)...) {}

  constexpr var_union(const var_union&) = default;
  constexpr var_union(var_union&&) = default;
  constexpr var_union& operator=(const var_union&) = default;
  constexpr var_union& operator=(var_union&&) = default;

  constexpr ~var_union() = default;

  constexpr ~var_union()
    requires (!is_triv_dtor)
  {}

  var_union<is_triv_dtor, get_first_types_t<TypeList<Types...>, sizeof...(Types) / 2>> fst;
  var_union<is_triv_dtor, get_rest_types_t<TypeList<Types...>, sizeof...(Types) / 2>> snd;
};

template <size_t N, size_t L, size_t R, typename Union>
constexpr decltype(auto) get_impl(Union&& u) {
  if constexpr (R - L == 1) {
    return std::forward<Union>(u).get();
  } else if constexpr (N < L + (R - L) / 2) {
    return get_impl<N, L, L + (R - L) / 2>(std::forward<Union>(u).fst);
  } else {
    return get_impl<N, L + (R - L) / 2, R>(std::forward<Union>(u).snd);
  }
}

template <size_t N, typename Variant>
constexpr decltype(auto) get_n(Variant&& v) {
  if (N != v.index()) {
    throw bad_variant_access();
  }
  return get_impl<N, 0, variant_size_v<std::remove_reference_t<Variant>>>(std::forward<Variant>(v).data);
}
} // namespace detail
