#pragma once

#include "tuple-traits.h"

#include <cstddef>
#include <memory>
#include <utility>

template <typename... Types>
class tuple;

template <typename... Types>
constexpr void swap(tuple<Types...>& lhs, tuple<Types...>& rhs);

template <typename T>
struct tuple_size;

template <typename... Types>
struct tuple_size<tuple<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <typename T>
inline constexpr std::size_t tuple_size_v = tuple_size<T>::value;

template <std::size_t N, typename T>
struct tuple_element;

template <std::size_t N, typename T>
using tuple_element_t = typename tuple_element<N, T>::type;

template <std::size_t N, typename... Types>
struct tuple_element<N, tuple<Types...>> {
  using type = detail::get_at_t<N, Types...>;
};

namespace detail {

template <size_t I, typename T>
struct tuple_elem {
  constexpr tuple_elem() noexcept(std::is_nothrow_default_constructible_v<T>)
    requires std::is_default_constructible_v<T>
      : elem{} {}

  constexpr tuple_elem(const T& t) noexcept(std::is_nothrow_copy_constructible_v<T>)
    requires (std::is_copy_constructible_v<T>)
      : elem(t) {}

  template <typename U>
  constexpr tuple_elem(U&& t) noexcept(std::is_nothrow_constructible_v<T, U>)
    requires (std::is_constructible_v<T, U>)
      : elem(std::forward<U>(t)) {}

  constexpr decltype(auto) get() && noexcept {
    return std::move(elem);
  }

  constexpr T& get() & noexcept {
    return elem;
  }

  constexpr const T&& get() const&& noexcept {
    return std::move(elem);
  }

  constexpr const T& get() const& noexcept {
    return elem;
  }

  T elem;
};

template <size_t I, typename T>
constexpr decltype(auto) get_impl(auto&& t) noexcept;

template <typename T, typename = std::make_index_sequence<tuple_size_v<T>>>
struct tuple_impl;

template <typename... Types, size_t... I>
struct tuple_impl<tuple<Types...>, std::index_sequence<I...>> : tuple_elem<I, Types>... {
  constexpr tuple_impl() noexcept
    requires (sizeof...(Types) == 0)
  = default;

  constexpr tuple_impl() noexcept((std::is_nothrow_default_constructible_v<Types> && ...))
    requires (sizeof...(Types) >= 1 && (std::is_default_constructible_v<Types> && ...))
  {}

  constexpr tuple_impl(const Types&... args) noexcept((std::is_nothrow_copy_constructible_v<Types> && ...))
    requires (sizeof...(Types) >= 1 && (std::is_copy_constructible_v<Types> && ...))
      : tuple_elem<I, Types>(args)... {}

  template <
      typename... UTypes,
      typename D = std::remove_cvref_t<detail::get_at_t<0, UTypes...>>,
      size_t SZ = sizeof...(UTypes)>
  constexpr tuple_impl(UTypes&&... args) noexcept((std::is_nothrow_constructible_v<Types, UTypes> && ...))
    requires (
        sizeof...(Types) == SZ && (std::is_constructible_v<Types, UTypes> && ...) &&
        (SZ >= 1 || (SZ == 1 && !detail::is_specialization_of_v<D, tuple>) )
    )
      : tuple_elem<I, Types>(std::forward<UTypes>(args))... {}

  template <typename... UTypes, size_t SZ = sizeof...(UTypes), size_t... IDX>
  constexpr tuple_impl(const tuple_impl<tuple<UTypes...>, std::index_sequence<IDX...>>& other
  ) noexcept((std::is_nothrow_constructible_v<Types, decltype(get_impl<I, UTypes>(other))> && ...))
    requires (
        sizeof...(Types) == SZ && (std::is_constructible_v<Types, decltype(get_impl<I, UTypes>(other))> && ...) &&
        (SZ != 1 || !(std::is_same_v<Types..., UTypes...> || std::is_convertible_v<decltype(other), Types...> ||
                      std::is_constructible_v<Types..., decltype(other)>) )
    )
      : tuple_elem<I, Types>(get_impl<I, UTypes>(other))... {}

  template <typename... UTypes, size_t SZ = sizeof...(UTypes), size_t... IDX>
  constexpr tuple_impl(tuple_impl<tuple<UTypes...>, std::index_sequence<IDX...>>&& other
  ) noexcept((std::is_nothrow_constructible_v<Types, decltype(get_impl<I, UTypes>(std::move(other)))> && ...))
    requires (
        sizeof...(Types) == SZ &&
        (std::is_constructible_v<Types, decltype(get_impl<I, UTypes>(std::move(other)))> && ...) &&
        (SZ != 1 || !(std::is_same_v<Types..., UTypes...> || std::is_convertible_v<decltype(other), Types...> ||
                      std::is_constructible_v<Types..., decltype(other)>) )
    )
      : tuple_elem<I, Types>(get_impl<I, UTypes>(std::move(other)))... {}
};

template <size_t I, typename T>
constexpr decltype(auto) get_impl(auto&& t) noexcept {
  return std::forward<decltype(t)>(t).template tuple_elem<I, T>::get();
}

template <size_t I, typename T>
constexpr decltype(auto) get_tuple_impl(auto&& t) noexcept {
  return get_impl<I, T>(t.as_base());
}

template <typename T, typename U>
constexpr auto three_way_compare(const T& t, const U& u) {
  if constexpr (std::three_way_comparable_with<T, U>) {
    return t <=> u;
  } else {
    if (t < u) {
      return std::weak_ordering::less;
    }
    if (u < t) {
      return std::weak_ordering::greater;
    }
    return std::weak_ordering::equivalent;
  }
}
} // namespace detail

template <typename... Types>
class tuple : detail::tuple_impl<tuple<Types...>> {
  using base = detail::tuple_impl<tuple>;

  template <typename... UTypes>
  friend class tuple;

public:
  constexpr tuple() noexcept
    requires (tuple_size_v<tuple> == 0)
  = default;

  constexpr explicit(!(detail::is_copy_list_constructible<Types> && ...)) tuple() noexcept(noexcept(base()))
    requires (tuple_size_v<tuple> >= 1 && std::is_default_constructible_v<base>)
      : base() {}

  constexpr explicit(!(std::is_convertible_v<const Types&, Types> && ...))
      tuple(const Types&... args) noexcept(noexcept(base(args...)))
    requires (sizeof...(Types) >= 1 && std::is_constructible_v<base, const Types&...>)
      : base(args...) {}

  template <typename... UTypes>
  constexpr tuple(UTypes&&... args) noexcept(noexcept(base(std::forward<UTypes>(args)...)))
    requires (std::is_constructible_v<base, UTypes && ...>)
      : base(std::forward<UTypes>(args)...) {}

  template <typename... UTypes>
  constexpr explicit(!(std::is_convertible_v<const UTypes&, Types> && ...))
      tuple(const tuple<UTypes...>& other) noexcept(noexcept(base(other.as_base())))
    requires (std::is_constructible_v<base, const detail::tuple_impl<tuple<UTypes...>>&>)
      : base(other.as_base()) {}

  template <typename... UTypes>
  constexpr explicit(!(std::is_convertible_v<std::remove_reference_t<UTypes>&&, Types> && ...))
      tuple(tuple<UTypes...>&& other) noexcept(noexcept(base(std::move(other.as_base()))))
    requires (std::is_constructible_v<base, detail::tuple_impl<tuple<UTypes...>> &&>)
      : base(std::move(other.as_base())) {}

private:
  template <size_t I, typename T>
  friend constexpr decltype(auto) detail::get_tuple_impl(auto&& t) noexcept;

  constexpr base& as_base() & noexcept {
    return *this;
  }

  constexpr const base& as_base() const& noexcept {
    return *this;
  }

  constexpr base&& as_base() && noexcept {
    return std::move(*this);
  }

  constexpr const base&& as_base() const&& noexcept {
    return std::move(*this);
  }
};

// deduction guide to aid CTAD
template <typename... Types>
tuple(Types...) -> tuple<Types...>;

template <typename... Types>
constexpr tuple<detail::deduce_vtype_t<Types>...> make_tuple(Types&&... args
) noexcept(noexcept(tuple<detail::deduce_vtype_t<Types>...>{std::forward<Types>(args)...})) {
  return {std::forward<Types>(args)...};
}

template <std::size_t N, typename... Types>
constexpr tuple_element_t<N, tuple<Types...>>& get(tuple<Types...>& t) noexcept
  requires (N < sizeof...(Types))
{
  return detail::get_tuple_impl<N, detail::get_at_t<N, Types...>>(t);
}

template <std::size_t N, typename... Types>
constexpr tuple_element_t<N, tuple<Types...>>&& get(tuple<Types...>&& t) noexcept
  requires (N < sizeof...(Types))
{
  return std::move(detail::get_tuple_impl<N, detail::get_at_t<N, Types...>>(std::move(t)));
}

template <std::size_t N, typename... Types>
constexpr const tuple_element_t<N, tuple<Types...>>& get(const tuple<Types...>& t) noexcept
  requires (N < sizeof...(Types))
{
  return detail::get_tuple_impl<N, detail::get_at_t<N, Types...>>(t);
}

template <std::size_t N, typename... Types>
constexpr const tuple_element_t<N, tuple<Types...>>&& get(const tuple<Types...>&& t) noexcept
  requires (N < sizeof...(Types))
{
  return std::move(detail::get_tuple_impl<N, detail::get_at_t<N, Types...>>(std::move(t)));
}

template <typename T, typename... Types>
constexpr T& get(tuple<Types...>& t) noexcept
  requires detail::is_unique_v<T, Types...>
{
  return detail::get_tuple_impl<detail::get_idx_v<T, Types...>, T>(t);
}

template <typename T, typename... Types>
constexpr T&& get(tuple<Types...>&& t) noexcept
  requires detail::is_unique_v<T, Types...>
{
  return std::move(detail::get_tuple_impl<detail::get_idx_v<T, Types...>, T>(std::move(t)));
}

template <typename T, typename... Types>
constexpr const T& get(const tuple<Types...>& t) noexcept
  requires detail::is_unique_v<T, Types...>
{
  return detail::get_tuple_impl<detail::get_idx_v<T, Types...>, T>(t);
}

template <typename T, typename... Types>
constexpr const T&& get(const tuple<Types...>&& t) noexcept
  requires detail::is_unique_v<T, Types...>
{
  return std::move(detail::get_tuple_impl<detail::get_idx_v<T, Types...>, T>(std::move(t)));
}

namespace detail {
template <typename... TTypes, typename... UTypes, size_t... I>
constexpr bool eq_impl(const tuple<TTypes...>& lhs, const tuple<UTypes...>& rhs, std::index_sequence<I...>) {
  return ((get<I>(lhs) == get<I>(rhs)) && ...);
}

template <typename R, typename... TTypes, typename... UTypes, size_t I0, size_t... I>
constexpr R cmp_impl(const tuple<TTypes...>& lhs, const tuple<UTypes...>& rhs, std::index_sequence<I0, I...>) {
  auto c = detail::three_way_compare(get<I0>(lhs), get<I0>(rhs));
  if (!std::is_eq(c)) {
    return c;
  }
  return cmp_impl<R>(lhs, rhs, std::index_sequence<I...>{});
}

template <typename R, typename... TTypes, typename... UTypes>
constexpr R
cmp_impl([[maybe_unused]] const tuple<TTypes...>& lhs, [[maybe_unused]] const tuple<UTypes...>& rhs, std::index_sequence<>) {
  return R::equivalent;
}

template <typename... Types, size_t... I>
constexpr void swap_impl(tuple<Types...>& lhs, tuple<Types...>& rhs, std::index_sequence<I...>) noexcept(
    (std::is_nothrow_swappable_v<Types> && ...)
)
  requires ((std::is_swappable_v<Types> && ...))
{
  using std::swap;
  (swap(get<I>(lhs), get<I>(rhs)), ...);
}
} // namespace detail

template <typename... TTypes, typename... UTypes>
constexpr bool operator==(const tuple<TTypes...>& lhs, const tuple<UTypes...>& rhs)
  requires (sizeof...(TTypes) == sizeof...(UTypes))
{
  return detail::eq_impl(lhs, rhs, std::make_index_sequence<sizeof...(TTypes)>{});
}

template <typename... TTypes, typename... UTypes>
constexpr auto operator<=>(const tuple<TTypes...>& lhs, const tuple<UTypes...>& rhs)
  requires (sizeof...(TTypes) == sizeof...(UTypes))
{
  using R = std::common_comparison_category_t<
      decltype(detail::three_way_compare(std::declval<TTypes&>(), std::declval<UTypes&>()))...>;
  return detail::cmp_impl<R>(lhs, rhs, std::make_index_sequence<sizeof...(TTypes)>{});
}

template <typename... Types>
constexpr void swap(tuple<Types...>& lhs, tuple<Types...>& rhs) noexcept((std::is_nothrow_swappable_v<Types> && ...))
  requires ((std::is_swappable_v<Types> && ...))
{
  detail::swap_impl(lhs, rhs, std::make_index_sequence<sizeof...(Types)>{});
}

namespace detail {
template <typename R, typename seq, typename... Tuples>
struct tuple_concat;

template <typename R, size_t... I, typename Head, typename... Tail>
struct tuple_concat<R, std::index_sequence<I...>, Head, Tail...> {
  template <typename... Obj>
  static constexpr R invoke(Head&& h, Tail&&... t, Obj&&... obj) {
    if constexpr (sizeof...(Tail) == 0) {
      return R{std::forward<Obj>(obj)..., get<I>(std::forward<Head>(h))...};
    } else {
      return tuple_concat<
          R,
          std::make_index_sequence<tuple_size_v<std::remove_cvref_t<detail::get_at_t<0, Tail...>>>>,
          Tail...>::invoke(std::forward<Tail>(t)..., std::forward<Obj>(obj)..., get<I>(std::forward<Head>(h))...);
    }
  }
};
} // namespace detail

template <typename... Tuples>
constexpr detail::merge_t<tuple<>, std::remove_cvref_t<Tuples>...> tuple_cat(Tuples&&... args) {
  using R = detail::merge_t<tuple<>, std::remove_cvref_t<Tuples>...>;
  if constexpr (sizeof...(Tuples) == 0) {
    return {};
  } else {
    return detail::tuple_concat<
        R,
        std::make_index_sequence<tuple_size_v<std::remove_cvref_t<detail::get_at_t<0, Tuples...>>>>,
        Tuples&&...>::invoke(std::forward<Tuples>(args)...);
  }
}
