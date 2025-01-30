#pragma once

#include "storage.h"
#include "variant-helper.h"
#include "variant-traits.h"
#include "visit-helper.h"

template <typename... Types>
  requires variant_concept<Types...>
class variant {
private:
  using traits = detail::traits<Types...>;

  template <size_t N, typename Variant>
  friend constexpr decltype(auto) detail::get_n(Variant&& v);

public:
  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<variant_alternative_t<0, variant>>)
    requires (std::is_default_constructible_v<variant_alternative_t<0, variant>>)
      : data(in_place_index<0>)
      , idx_{0} {}

  constexpr variant(const variant& other) noexcept
    requires detail::var_triv_copy_ctor<Types...>
  = default;

  constexpr variant(const variant& other) noexcept(traits::is_nothrow_copy_constructible_v)
    requires detail::var_copy_ctor<Types...>
  {
    if (!other.valueless_by_exception()) {
      detail::visit_impl<void>(
          [this]<size_t I, typename V>(std::index_sequence<I>, V&& val) {
            this->template emplace<I>(std::forward<V>(val));
          },
          other
      );
      idx_ = other.idx_;
    }
  }

  constexpr variant(variant&& other) noexcept
    requires detail::var_triv_move_ctor<Types...>
  = default;

  constexpr variant(variant&& other) noexcept(traits::is_nothrow_move_constructible_v)
    requires detail::var_move_ctor<Types...>
  {
    if (!other.valueless_by_exception()) {
      detail::visit_impl<void>(
          [this]<size_t I, typename V>(std::index_sequence<I>, V&& val) {
            this->template emplace<I>(std::forward<V>(val));
          },
          std::move(other)
      );
      idx_ = other.idx_;
    }
  }

  constexpr variant& operator=(const variant& other) noexcept
    requires detail::var_triv_copy_assignment<Types...>
  = default;

  constexpr variant& operator=(const variant& other) noexcept(
      traits::is_nothrow_copy_assignable_v && traits::is_nothrow_copy_constructible_v &&
      traits::is_nothrow_move_constructible_v
  )
    requires detail::var_copy_assignment<Types...>
  {
    if (this == &other) {
      return *this;
    }
    if (other.valueless_by_exception()) {
      reset();
    } else {
      detail::visit_impl<void>(
          [this,
           &other]<size_t I, size_t J>(std::index_sequence<I, J>, auto&& other_val, [[maybe_unused]] auto&& this_val) {
            if constexpr (I == J) {
              get<I>(*this) = std::forward<decltype(other_val)>(other_val);
            } else {
              using T_cur = variant_alternative_t<I, variant>;
              if constexpr (std::is_nothrow_copy_constructible_v<T_cur> ||
                            !std::is_nothrow_move_constructible_v<T_cur>) {
                this->template emplace<I>(std::forward<decltype(other_val)>(other_val));
              } else {
                this->operator=(variant(other));
              }
            }
          },
          other,
          *this
      );
    }
    return *this;
  }

  constexpr variant& operator=(variant&& other) noexcept
    requires detail::var_triv_move_assignment<Types...>
  = default;

  constexpr variant& operator=(variant&& other
  ) noexcept(traits::is_nothrow_move_constructible_v && traits::is_nothrow_move_assignable_v)
    requires detail::var_move_assignment<Types...>
  {
    if (other.valueless_by_exception()) {
      reset();
    } else {
      detail::visit_impl<void>(
          [this]<size_t I>(std::index_sequence<I>, auto&& val) {
            if (this->index() == I) {
              get<I>(*this) = std::forward<decltype(val)>(val);
            } else {
              this->template emplace<I>(std::forward<decltype(val)>(val));
            }
          },
          std::move(other)
      );
    }
    return *this;
  }

  template <typename T, typename... Args>
  constexpr explicit variant(
      in_place_type_t<T>,

      Args&&... args
  ) noexcept(std::is_nothrow_constructible_v<T, Args...>)
    requires (detail::is_unique_v<T, Types...> && std::is_constructible_v<T, Args...>)
      : variant(in_place_index<detail::get_idx_v<T, Types...>>, std::forward<Args>(args)...) {}

  template <typename T, typename U, typename... Args>
  constexpr explicit variant(
      in_place_type_t<T>,
      std::initializer_list<U> il,

      Args&&... args
  ) noexcept(std::is_nothrow_constructible_v<T, std::initializer_list<U>&, Args...>)
    requires (detail::is_unique_v<T, Types...> && std::is_constructible_v<T, std::initializer_list<U>&, Args...>)
      : variant(std::in_place_index<detail::get_idx_v<T, Types...>>, il, std::forward<Args>(args)...) {}

  template <size_t I, typename... Args>
  constexpr explicit variant(
      in_place_index_t<I> i,

      Args&&... args
  ) noexcept(std::is_nothrow_constructible_v<variant_alternative_t<I, variant>, Args...>)
    requires (I < variant_size_v<variant> && std::is_constructible_v<variant_alternative_t<I, variant>, Args...>)
      : data{i, std::forward<Args>(args)...}
      , idx_{I} {}

  template <size_t I, typename U, typename... Args>
  constexpr explicit variant(
      in_place_index_t<I> i,
      std::initializer_list<U> il,

      Args&&... args
  ) noexcept(std::is_nothrow_constructible_v<variant_alternative_t<I, variant>, std::initializer_list<U>&, Args...>)
    requires (I < variant_size_v<variant> &&
              std::is_constructible_v<variant_alternative_t<I, variant>, std::initializer_list<U>&, Args...>)
      : data{i, il, std::forward<Args>(args)...}
      , idx_{I} {}

  template <typename T, typename T_j = detail::type_resolve_t<T, Types...>>
  constexpr variant(T&& t) noexcept(std::is_nothrow_constructible_v<T_j, T>)
    requires (!(std::is_same_v<std::remove_cvref_t<T>, variant> || detail::is_in_place_tag_v<T>) && std::is_constructible_v<T_j, T> && detail::is_unique_v<T_j, Types...>)
      : variant(in_place_type_t<T_j>{}, std::forward<T>(t)) {}

  template <typename T, typename T_j = detail::type_resolve_t<T, Types...>>
  variant& operator=(T&& t) noexcept(std::is_nothrow_constructible_v<T_j, T> && std::is_nothrow_assignable_v<T_j, T>)
    requires (!(std::is_same_v<std::remove_cvref_t<T>, variant> || detail::is_in_place_tag_v<T>) && std::is_constructible_v<T_j, T> && detail::is_unique_v<T_j, Types...>)
  {
    if (constexpr size_t J = detail::type_resolve_idx<T, Types...>; J == index()) {
      get<J>(*this) = std::forward<T>(t);
    } else {
      if constexpr (std::is_nothrow_constructible_v<T_j, T> || !std::is_nothrow_move_constructible_v<T_j>) {
        emplace<J>(std::forward<T>(t));
      } else {
        emplace<J>(T_j(std::forward<T>(t)));
      }
    }
    return *this;
  }

  constexpr ~variant()
    requires (traits::is_trivially_destructible_v)
  = default;

  constexpr ~variant()
    requires (!traits::is_trivially_destructible_v)
  {
    if (!valueless_by_exception()) {
      reset();
    }
  }

  template <class T, class... Args>
  constexpr T& emplace(Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args...>)
    requires (std::is_constructible_v<T, Args...> && detail::is_unique_v<T, Types...>)
  {
    return emplace<detail::get_idx_v<T, Types...>>(std::forward<Args>(args)...);
  }

  template <class T, class U, class... Args>
  constexpr T& emplace(
      std::initializer_list<U> il,
      Args&&... args
  ) noexcept(std::is_nothrow_constructible_v<T, std::initializer_list<U>&, Args...>)
    requires (std::is_constructible_v<T, std::initializer_list<U>&, Args...> && detail::is_unique_v<T, Types...>)

  {
    return emplace<detail::get_idx_v<T, Types...>>(il, std::forward<Args>(args)...);
  }

  template <size_t I, class... Args>
  constexpr variant_alternative_t<I, variant>& emplace(Args&&... args
  ) noexcept(std::is_nothrow_constructible_v<variant_alternative_t<I, variant>, Args...>)
    requires (std::is_constructible_v<variant_alternative_t<I, variant>, Args...>)
  {
    reset();
    std::construct_at(&data, in_place_index<I>, std::forward<Args>(args)...);
    idx_ = I;
    return get<I>(*this);
  }

  template <size_t I, class U, class... Args>
  constexpr variant_alternative_t<I, variant>& emplace(
      std::initializer_list<U> il,
      Args&&... args
  ) noexcept(std::is_nothrow_constructible_v<variant_alternative_t<I, variant>, std::initializer_list<U>&, Args...>)
    requires (std::is_constructible_v<variant_alternative_t<I, variant>, std::initializer_list<U>&, Args...>)
  {
    reset();
    std::construct_at(std::addressof(this->data), in_place_index<I>, il, std::forward<Args>(args)...);
    idx_ = I;
    return get<I>(*this);
  }

  constexpr size_t index() const noexcept {
    return idx_;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return idx_ == variant_npos;
  }

  void swap(variant& other) noexcept(traits::is_nothrow_move_constructible_v && traits::is_nothrow_swappable_v)
    requires (traits::is_swappable_v && traits::is_move_constructible_v)
  {
    if (other.valueless_by_exception()) {
      if (!valueless_by_exception()) {
        other.swap(*this);
      }
      return;
    }
    if (valueless_by_exception()) {
      detail::visit_impl<void>(
          [this]<size_t I>(std::index_sequence<I>, auto&& val) { this->template emplace<I>(std::move(val)); },
          other
      );
      other.reset();
    } else {
      detail::visit_impl<void>(
          [this, &other]<size_t I, size_t J>(std::index_sequence<I, J>, auto&& lhs, auto&& rhs) {
            if constexpr (I == J) {
              using std::swap;
              swap(lhs, rhs);
            } else {
              auto tmp = std::move(rhs);
              other.template emplace<I>(std::move(lhs));
              this->template emplace<J>(std::move(tmp));
            }
          },
          *this,
          other
      );
    }
  }

private:
  constexpr void reset() {
    if (valueless_by_exception()) {
      return;
    }
    detail::visit_impl<void>(
        []([[maybe_unused]] auto&& seq, auto&& value) { std::destroy_at(std::addressof(value)); },
        *this
    );
    idx_ = variant_npos;
  }

  detail::var_union<traits::is_trivially_destructible_v, detail::TypeList<Types...>> data;
  size_t idx_{variant_npos};
};

template <typename... Types>
constexpr void swap(variant<Types...>& lhs, variant<Types...>& rhs) noexcept(
    detail::traits<Types...>::is_nothrow_move_constructible_v && detail::traits<Types...>::is_nothrow_swappable_v
)
  requires (detail::traits<Types...>::is_move_constructible_v && detail::traits<Types...>::is_swappable_v)
{
  lhs.swap(rhs);
}

template <typename... Types>
constexpr void swap(variant<Types...>& lhs, variant<Types...>& rhs) noexcept(
    detail::traits<Types...>::is_nothrow_move_constructible_v && detail::traits<Types...>::is_nothrow_swappable_v
)
  requires (!(detail::traits<Types...>::is_move_constructible_v && detail::traits<Types...>::is_swappable_v))
= delete;

template <typename T, typename... Types>
constexpr bool holds_alternative(const variant<Types...>& v) noexcept
  requires (detail::is_unique_v<T, Types...>)
{
  return v.index() == detail::get_idx_v<T, Types...>;
}

template <size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v)
  requires (I < variant_size_v<variant<Types...>>)
{
  return detail::get_n<I>(v);
}

template <size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v)
  requires (I < variant_size_v<variant<Types...>>)
{
  return detail::get_n<I>(std::move(v));
}

template <size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant<Types...>>& get(const variant<Types...>& v)
  requires (I < variant_size_v<variant<Types...>>)
{
  return detail::get_n<I>(v);
}

template <size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant<Types...>>&& get(const variant<Types...>&& v)
  requires (I < variant_size_v<variant<Types...>>)
{
  return detail::get_n<I>(std::move(v));
}

template <typename T, typename... Types>
constexpr T& get(variant<Types...>& v)
  requires (detail::is_unique_v<T, Types...>)
{
  return get<detail::get_idx_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr T&& get(variant<Types...>&& v)
  requires (detail::is_unique_v<T, Types...>)
{
  return get<detail::get_idx_v<T, Types...>>(std::move(v));
}

template <typename T, typename... Types>
constexpr const T& get(const variant<Types...>& v)
  requires (detail::is_unique_v<T, Types...>)
{
  return get<detail::get_idx_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr const T&& get(const variant<Types...>&& v)
  requires (detail::is_unique_v<T, Types...>)
{
  return get<detail::get_idx_v<T, Types...>>(std::move(v));
}

template <size_t I, class... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept
  requires (I < sizeof...(Types))
{
  return pv && pv->index() == I ? std::addressof(get<I>(*pv)) : nullptr;
}

template <size_t I, class... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>> get_if(const variant<Types...>* pv
) noexcept
  requires (I < sizeof...(Types))
{
  return pv && pv->index() == I ? std::addressof(get<I>(*pv)) : nullptr;
}

template <class T, class... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept
  requires (detail::is_unique_v<T, Types...>)
{
  return get_if<detail::get_idx_v<T, Types...>>(pv);
}

template <class T, class... Types>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept
  requires (detail::is_unique_v<T, Types...>)
{
  return get_if<detail::get_idx_v<T, Types...>>(pv);
}

template <typename R, typename Visitor, typename... Variants>
constexpr R visit(Visitor&& vis, Variants&&... vars) {
  return detail::visit_impl<R>(
      [&vis]<size_t... I>(std::index_sequence<I...>, auto&&... args) -> R {
        return detail::invoke_r<R>(std::forward<Visitor>(vis), std::forward<decltype(args)>(args)...);
      },
      std::forward<Variants>(vars)...
  );
}

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars) {
  using R = std::invoke_result_t<Visitor, decltype(get<0>(std::declval<Variants>()))...>;
  return detail::visit_impl<R>(
      [&vis]<size_t... I>(std::index_sequence<I...>, auto&&... args) -> R {
        static_assert(std::is_same_v<R, std::invoke_result_t<Visitor, decltype(args)...>>);
        return std::invoke(std::forward<Visitor>(vis), std::forward<decltype(args)>(args)...);
      },
      std::forward<Variants>(vars)...
  );
}

namespace detail {

template <typename... Types, typename Comp>
bool comp_impl(const variant<Types...>& lhs, const variant<Types...>& rhs, Comp comp) {
  if (rhs.index() != variant_npos && lhs.index() == rhs.index()) {
    return detail::visit_impl<bool>(
        [&comp, &rhs]<size_t I>(std::index_sequence<I>, auto&& val1) { return comp(val1, get<I>(rhs)); },
        lhs
    );
  } else {
    return comp(lhs.index() + 1, rhs.index() + 1);
  }
}
} // namespace detail

template <class... Types>
constexpr bool operator==(
    const variant<Types...>& v,

    const variant<Types...>& w
) {
  return detail::comp_impl(v, w, std::equal_to{});
}

template <class... Types>
constexpr bool operator!=(
    const variant<Types...>& v,

    const variant<Types...>& w
) {
  return detail::comp_impl(v, w, std::not_equal_to{});
}

template <class... Types>
constexpr bool operator<(
    const variant<Types...>& v,

    const variant<Types...>& w
) {
  return detail::comp_impl(v, w, std::less{});
}

template <class... Types>
constexpr bool operator>(
    const variant<Types...>& v,

    const variant<Types...>& w
) {
  return detail::comp_impl(v, w, std::greater{});
}

template <class... Types>
constexpr bool operator<=(
    const variant<Types...>& v,

    const variant<Types...>& w
) {
  return detail::comp_impl(v, w, std::less_equal{});
}

template <class... Types>
constexpr bool operator>=(
    const variant<Types...>& v,

    const variant<Types...>& w
) {
  return detail::comp_impl(v, w, std::greater_equal{});
}

template <class... Types>
constexpr std::common_comparison_category_t<std::compare_three_way_result_t<Types>...> operator<=>(
    const variant<Types...>& v,

    const variant<Types...>& w
) {
  if (w.valueless_by_exception() && v.valueless_by_exception()) {
    return std::strong_ordering::equal;
  }
  if (v.valueless_by_exception()) {
    return std::strong_ordering::less;
  }
  if (w.valueless_by_exception()) {
    return std::strong_ordering::greater;
  }
  if (v.index() != w.index()) {
    return v.index() <=> w.index();
  }
  return visit<std::common_comparison_category_t<std::compare_three_way_result_t<Types>...>>(
      [](auto&& val1, auto&& val2) { return val1 <=> val2; },
      v,
      w
  );
}
