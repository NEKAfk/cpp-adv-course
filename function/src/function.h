#pragma once
#include <cstddef>
#include <exception>
#include <functional>
#include <memory>
#include <string>
#include <type_traits>
#include <typeindex>
#include <utility>

class bad_function_call final : public std::exception {
public:
  bad_function_call() noexcept = default;
  bad_function_call(const bad_function_call& other) noexcept = default;
  bad_function_call& operator=(const bad_function_call& other) noexcept = default;
  ~bad_function_call() noexcept override = default;

  const char* what() const noexcept override {
    return "call no instance function";
  }
};

template <typename F>
class function;

namespace detail {

template <typename F, size_t SMALL_SIZE>
concept is_small =
    alignof(F) <= alignof(std::max_align_t) && sizeof(F) <= SMALL_SIZE && std::is_nothrow_move_constructible_v<F>;
} // namespace detail

template <typename R, typename... Args>
class function<R(Args...)> {
  static constexpr std::size_t SMALL_SIZE = sizeof(void*) * 4;

  struct storage {
    alignas(std::max_align_t) std::byte data[SMALL_SIZE];
  };

  struct basic_operations {
    basic_operations() = default;

    virtual R call(storage& s, Args&&... args) const = 0;

    virtual void copy(storage& dst, const storage& src) const = 0;

    virtual void move(storage& dst, storage& src) const noexcept = 0;

    virtual void destroy(storage& s) const noexcept = 0;
  };

  template <typename F>
  struct operations;

  template <typename F>
    requires (!detail::is_small<F, SMALL_SIZE> && !std::is_same_v<F, bad_function_call>)
  struct operations<F> final : basic_operations {
    template <typename FType>
    static void construct(storage& s, FType&& f) {
      std::construct_at(reinterpret_cast<F**>(&s.data[0]), new F(std::forward<FType>(f)));
    }

    static F* get_target(storage& s) {
      return *std::launder(reinterpret_cast<F**>(&s.data[0]));
    }

    static const F* get_target(const storage& s) {
      return *std::launder(reinterpret_cast<const F* const*>(&s.data[0]));
    }

    R call(storage& s, Args&&... args) const override {
      return (*get_target(s))(std::forward<Args>(args)...);
    }

    void copy(storage& dst, const storage& src) const override {
      construct(dst, *get_target(src));
    }

    void move(storage& dst, storage& src) const noexcept override {
      std::construct_at(reinterpret_cast<F**>(&dst.data[0]), get_target(src));
    }

    void destroy(storage& s) const noexcept override {
      auto ptr = std::launder(reinterpret_cast<F**>(&s.data[0]));
      delete *ptr;
      std::destroy_at(ptr);
    }
  };

  template <detail::is_small<SMALL_SIZE> F>
    requires (!std::is_same_v<F, bad_function_call>)
  struct operations<F> final : basic_operations {
    template <typename FType>
    static void construct(storage& s, FType&& f) {
      std::construct_at(reinterpret_cast<F*>(&s.data[0]), std::forward<FType>(f));
    }

    R call(storage& s, Args&&... args) const override {
      return (*get_target(s))(std::forward<Args>(args)...);
    }

    static F* get_target(storage& s) {
      return std::launder(reinterpret_cast<F*>(&s.data[0]));
    }

    static const F* get_target(const storage& s) {
      return std::launder(reinterpret_cast<const F*>(&s.data[0]));
    }

    void copy(storage& dst, const storage& src) const override {
      construct(dst, *get_target(src));
    }

    void move(storage& dst, storage& src) const noexcept override {
      construct(dst, std::move(*get_target(src)));
      destroy(src);
    }

    void destroy(storage& s) const noexcept override {
      std::destroy_at(get_target(s));
    }
  };

  template <std::same_as<bad_function_call> F>
  struct operations<F> final : basic_operations {
    R call([[maybe_unused]] storage& s, [[maybe_unused]] Args&&... args) const override {
      throw bad_function_call();
    }

    void copy([[maybe_unused]] storage& dst, [[maybe_unused]] const storage& src) const override {}

    void move([[maybe_unused]] storage& dst, [[maybe_unused]] storage& src) const noexcept override {}

    void destroy([[maybe_unused]] storage& s) const noexcept override {}

    static F* get_target([[maybe_unused]] storage& s) {
      return nullptr;
    }
  };

public:
  function() noexcept
      : _ops(&OPS<bad_function_call>) {}

  template <typename F>
  function(F func)
    requires std::is_invocable_r_v<R, F, Args...>
      : _ops(&OPS<F>) {
    OPS<F>.construct(_storage, std::move(func));
  }

  function(const function& other)
      : _ops(other._ops) {
    _ops->copy(_storage, other._storage);
  }

  function(function&& other) noexcept
      : _ops(std::exchange(other._ops, &OPS<bad_function_call>)) {
    _ops->move(_storage, other._storage);
  }

  function& operator=(const function& other) {
    if (this != &other) {
      function tmp{other};
      *this = std::move(tmp);
    }
    return *this;
  }

  function& operator=(function&& other) noexcept {
    if (this != &other) {
      _ops->destroy(_storage);
      _ops = other._ops;
      _ops->move(_storage, other._storage);
      other._ops = &OPS<bad_function_call>;
    }
    return *this;
  }

  ~function() {
    _ops->destroy(_storage);
  }

  explicit operator bool() const noexcept {
    return _ops != &OPS<bad_function_call>;
  }

  R operator()(Args... args) const {
    return _ops->call(_storage, std::forward<Args>(args)...);
  }

  template <typename T>
  T* target() noexcept {
    return const_cast<T*>(std::as_const(*this).template target<T>());
  }

  template <typename T>
  const T* target() const noexcept {
    if (&OPS<T> != _ops) {
      return nullptr;
    }
    return static_cast<operations<T>*>(_ops)->get_target(_storage);
  }

private:
  mutable storage _storage{};
  basic_operations* _ops;

  template <typename F>
  inline static operations<F> OPS;
};
