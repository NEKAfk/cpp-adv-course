#pragma once
#include <memory>
#include <type_traits>

namespace detail {
template <typename T, bool is_trivial = std::is_trivially_destructible_v<T>>
struct dtor_base {
  constexpr dtor_base() noexcept {}

  constexpr dtor_base(const dtor_base&) = default;

  constexpr dtor_base(dtor_base&&) = default;

  constexpr dtor_base& operator=(const dtor_base&) = default;

  constexpr dtor_base& operator=(dtor_base&&) = default;

  constexpr ~dtor_base() {
    reset();
  }

  constexpr void reset() noexcept {
    if (is_active) {
      std::destroy_at(std::addressof(obj));
    }
    is_active = false;
  }

protected:
  union {
    T obj;
  };

  bool is_active{false};
};

template <typename T>
struct dtor_base<T, true> {
  constexpr dtor_base() noexcept {}

  constexpr dtor_base(const dtor_base&) = default;

  constexpr dtor_base(dtor_base&&) = default;

  constexpr dtor_base& operator=(const dtor_base&) = default;

  constexpr dtor_base& operator=(dtor_base&&) = default;

  constexpr ~dtor_base() = default;

  constexpr void reset() noexcept {
    is_active = false;
  }

protected:
  union {
    T obj;
  };

  bool is_active{false};
};
} // namespace detail
