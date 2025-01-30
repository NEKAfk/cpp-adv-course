#pragma once

#include "variant-helper.h"
#include "variant-traits.h"

#include <functional>
#include <optional>

namespace detail {

template <class R, class F, class... Args>
  requires std::is_invocable_r_v<R, F, Args...>
constexpr R invoke_r(F&& f, Args&&... args) noexcept(std::is_nothrow_invocable_r_v<R, F, Args...>) {
  if constexpr (std::is_void_v<R>) {
    std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
  } else {
    return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
  }
}

template <typename T, size_t... DIM>
struct multi_array;

template <typename T>
struct multi_array<T> {
  explicit constexpr multi_array(T func)
      : data{func} {}

  constexpr decltype(auto) operator()() const {
    return data;
  }

  T data;
};

template <typename T, size_t FST, size_t... REST>
struct multi_array<T, FST, REST...> {
  using ArrType = multi_array<T, REST...>;

  template <std::same_as<ArrType>... S>
  constexpr multi_array(S... arrs)
      : arr{arrs...} {}

  template <std::same_as<size_t>... S>
  constexpr decltype(auto) operator()(size_t head, S... tail) const {
    return arr[head](tail...);
  }

  multi_array<T, REST...> arr[FST];
};

template <typename ArrType, typename Seq>
struct create_visit_table_impl;

template <typename R, typename Visitor, typename... Variants, size_t FIRST_DIM, size_t... REST_DIM, size_t... HEAD>
struct create_visit_table_impl<
    multi_array<R (*)(Visitor, Variants...), FIRST_DIM, REST_DIM...>,
    std::index_sequence<HEAD...>> {
  template <size_t... CUR, typename... Tail>
  static constexpr auto invoke(std::index_sequence<CUR...>, Tail&&... tail) {
    return multi_array<R (*)(Visitor, Variants...), FIRST_DIM, REST_DIM...>(
        create_visit_table_impl<
            multi_array<R (*)(Visitor, Variants...), REST_DIM...>,
            std::index_sequence<HEAD..., CUR>>::invoke(std::forward<Tail>(tail)...)...
    );
  }
};

template <typename R, typename Visitor, typename... Variants, size_t... I>
struct create_visit_table_impl<multi_array<R (*)(Visitor, Variants...)>, std::index_sequence<I...>> {
  static constexpr R visit_invoke(Visitor visitor, Variants... vars) {
    return detail::invoke_r<R>(
        std::forward<Visitor>(visitor),
        std::index_sequence<I...>{},
        get<I>(std::forward<Variants>(vars))...
    );
  }

  static constexpr decltype(auto) invoke() {
    return multi_array<R (*)(Visitor, Variants...)>{&visit_invoke};
  }
};

template <typename R, typename Visitor, typename... Variants>
struct array_wrapper {
  using ArrType = multi_array<R (*)(Visitor, Variants...), variant_size_v<std::remove_reference_t<Variants>>...>;
  static constexpr ArrType arr{create_visit_table_impl<ArrType, std::index_sequence<>>::invoke(
      std::make_index_sequence<variant_size_v<std::remove_reference_t<Variants>>>{}...
  )};
};

template <typename R, typename Visitor, typename... Variants>
constexpr R visit_impl(Visitor&& vis, Variants&&... vars) {
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  return detail::array_wrapper<R, Visitor&&, Variants&&...>::arr(vars.index(
  )...)(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}
} // namespace detail
