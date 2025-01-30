#pragma once

#include <algorithm>
#include <cstddef>
#include <memory>
#include <utility>

#ifdef _MSC_VER
#define NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#else
#define NO_UNIQUE_ADDRESS [[no_unique_address]]
#endif

template <typename T, typename Y>
concept pointer_convertible = std::is_convertible_v<T*, Y*>;

namespace block_detail {
struct control_block {
public:
  control_block()
      : strong_ref_count(0)
      , weak_ref_count(0) {}

  std::size_t ref_count() const noexcept {
    return strong_ref_count;
  }

  void decrease_strong_ref() noexcept {
    if (--strong_ref_count == 0) {
      die();
      if (weak_ref_count == 0) {
        delete this;
      }
    }
  }

  void increase_strong_ref() noexcept {
    ++strong_ref_count;
  }

  void decrease_weak_ref() noexcept {
    if (--weak_ref_count == 0 && strong_ref_count == 0) {
      delete this;
    }
  }

  void increase_weak_ref() noexcept {
    ++weak_ref_count;
  }

  virtual ~control_block() = default;

private:
  virtual void die() noexcept = 0;

  std::size_t strong_ref_count;
  std::size_t weak_ref_count;
};

template <typename T, typename Deleter>
struct ptr_control_block : control_block {
public:
  ptr_control_block(T* ptr, Deleter deleter)
      : ptr_(ptr)
      , deleter_(std::move(deleter)) {}

  ~ptr_control_block() override {}

private:
  void die() noexcept override {
    deleter_(ptr_);
  }

  T* ptr_;
  NO_UNIQUE_ADDRESS Deleter deleter_;
};

template <typename T>
struct obj_control_block : control_block {
public:
  template <typename... Args>
  explicit obj_control_block(Args&&... args)
      : obj_(std::forward<Args>(args)...) {}

  T* get_ptr() {
    return &obj_;
  }

  ~obj_control_block() override {}

private:
  void die() noexcept override {
    obj_.~T();
  }

  union {
    T obj_;
  };
};
} // namespace block_detail

template <typename T>
class weak_ptr;

template <typename T>
class shared_ptr {
  template <typename Y, typename... Args>
  friend shared_ptr<Y> make_shared(Args&&... args);

  template <typename Y>
  friend class shared_ptr;

  template <typename Y>
  friend class weak_ptr;

public:
  shared_ptr() noexcept
      : control_block_(nullptr)
      , data_(nullptr) {}

  shared_ptr(std::nullptr_t) noexcept
      : shared_ptr() {}

  template <pointer_convertible<T> Y>
  explicit shared_ptr(Y* ptr)
      : shared_ptr(ptr, std::default_delete<Y>{}) {}

  template <pointer_convertible<T> Y, typename Deleter>
  shared_ptr(Y* ptr, Deleter deleter) {
    try {
      control_block_ = new block_detail::ptr_control_block(ptr, std::move(deleter));
      control_block_->increase_strong_ref();
    } catch (...) {
      deleter(ptr);
      throw;
    }
    data_ = static_cast<T*>(ptr);
  }

  template <typename Y>
  shared_ptr(const shared_ptr<Y>& other, T* ptr) noexcept
      : shared_ptr(other.control_block_, ptr) {}

  template <typename Y>
  shared_ptr(shared_ptr<Y>&& other, T* ptr) noexcept
      : control_block_{std::exchange(other.control_block_, nullptr)}
      , data_{ptr} {
    other.data_ = nullptr;
  }

  shared_ptr(const shared_ptr& other) noexcept
      : shared_ptr(other, other.get()) {}

  template <pointer_convertible<T> Y>
  shared_ptr(const shared_ptr<Y>& other) noexcept
      : shared_ptr(other, static_cast<T*>(other.get())) {}

  shared_ptr(shared_ptr&& other) noexcept
      : shared_ptr(std::move(other), other.get()) {}

  template <pointer_convertible<T> Y>
  shared_ptr(shared_ptr<Y>&& other) noexcept
      : shared_ptr(std::move(other), other.get()) {}

  shared_ptr& operator=(const shared_ptr& other) noexcept {
    shared_ptr(other).swap(*this);
    return *this;
  }

  template <pointer_convertible<T> Y>
  shared_ptr& operator=(const shared_ptr<Y>& other) noexcept {
    return *this = shared_ptr(other);
  }

  shared_ptr& operator=(shared_ptr&& other) noexcept {
    if (this != &other) {
      swap(other);
      shared_ptr().swap(other);
    }
    return *this;
  }

  template <pointer_convertible<T> Y>
  shared_ptr& operator=(shared_ptr<Y>&& other) noexcept {
    return *this = shared_ptr(std::move(other));
  }

  T* get() const noexcept {
    return data_;
  }

  operator bool() const noexcept {
    return get() != nullptr;
  }

  T& operator*() const noexcept {
    return *get();
  }

  T* operator->() const noexcept {
    return get();
  }

  std::size_t use_count() const noexcept {
    return control_block_ ? control_block_->ref_count() : 0;
  }

  void reset() noexcept {
    *this = shared_ptr();
  }

  template <typename Y>
  void reset(Y* new_ptr) {
    *this = shared_ptr(new_ptr);
  }

  template <typename Y, typename Deleter>
  void reset(Y* new_ptr, Deleter deleter) {
    *this = shared_ptr(new_ptr, std::move(deleter));
  }

  friend bool operator==(const shared_ptr& lhs, const shared_ptr& rhs) noexcept {
    return lhs.get() == rhs.get();
  }

  void swap(shared_ptr& other) noexcept {
    std::swap(control_block_, other.control_block_);
    std::swap(data_, other.data_);
  }

  ~shared_ptr() noexcept {
    if (control_block_) {
      control_block_->decrease_strong_ref();
    }
  }

private:
  shared_ptr(block_detail::control_block* block, T* ptr)
      : control_block_(block)
      , data_(ptr) {
    if (control_block_) {
      control_block_->increase_strong_ref();
    }
  }

  block_detail::control_block* control_block_;
  T* data_;
};

template <typename T>
class weak_ptr {
  template <typename Y>
  friend class weak_ptr;

public:
  weak_ptr() noexcept
      : control_block_(nullptr)
      , data_(nullptr) {}

  template <pointer_convertible<T> Y>
  weak_ptr(const shared_ptr<Y>& other) noexcept
      : weak_ptr(other.control_block_, static_cast<T*>(other.get())) {}

  weak_ptr(const weak_ptr& other) noexcept
      : weak_ptr(other.control_block_, other.data_) {}

  template <pointer_convertible<T> Y>
  weak_ptr(const weak_ptr<Y>& other) noexcept
      : weak_ptr(other.control_block_, static_cast<T*>(other.data_)) {}

  weak_ptr(weak_ptr&& other) noexcept
      : control_block_{std::exchange(other.control_block_, nullptr)}
      , data_{std::exchange(other.data_, nullptr)} {}

  template <pointer_convertible<T> Y>
  weak_ptr(weak_ptr<Y>&& other) noexcept
      : control_block_{std::exchange(other.control_block_, nullptr)}
      , data_{static_cast<T*>(std::exchange(other.data_, nullptr))} {}

  template <pointer_convertible<T> Y>
  weak_ptr& operator=(const shared_ptr<Y>& other) noexcept {
    return *this = weak_ptr(other);
  }

  weak_ptr& operator=(const weak_ptr& other) noexcept {
    weak_ptr(other).swap(*this);
    return *this;
  }

  template <pointer_convertible<T> Y>
  weak_ptr& operator=(const weak_ptr<Y>& other) noexcept {
    return *this = weak_ptr(other);
  }

  weak_ptr& operator=(weak_ptr&& other) noexcept {
    if (this != &other) {
      swap(other);
      weak_ptr().swap(other);
    }
    return *this;
  }

  template <pointer_convertible<T> Y>
  weak_ptr& operator=(weak_ptr<Y>&& other) noexcept {
    return *this = weak_ptr(std::move(other));
  }

  shared_ptr<T> lock() const noexcept {
    return !control_block_ || control_block_->ref_count() == 0 ? shared_ptr<T>() : shared_ptr(control_block_, data_);
  }

  void reset() noexcept {
    *this = weak_ptr();
  }

  void swap(weak_ptr& other) noexcept {
    std::swap(control_block_, other.control_block_);
    std::swap(data_, other.data_);
  }

  ~weak_ptr() {
    if (control_block_) {
      control_block_->decrease_weak_ref();
    }
  }

private:
  weak_ptr(block_detail::control_block* block, T* ptr)
      : control_block_(block)
      , data_(ptr) {
    if (control_block_) {
      control_block_->increase_weak_ref();
    }
  }

  block_detail::control_block* control_block_;
  T* data_;
};

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args&&... args) {
  auto* control_block_ = new block_detail::obj_control_block<T>(std::forward<Args>(args)...);
  return shared_ptr<T>(static_cast<block_detail::control_block*>(control_block_), control_block_->get_ptr());
}
