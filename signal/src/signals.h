#pragma once

#include "intrusive-list.h"

#include <functional>
#include <utility>

namespace signals {

template <typename T>
class signal;

template <typename... Args>
class signal<void(Args...)> {
public:
  using slot = std::function<void(Args...)>;

  class connection;
  struct signal_tag;

private:
  struct iterator_token;

public:
  using connection_list = intrusive::list<connection, signal_tag>;
  using list_const_iterator = typename connection_list::const_iterator;

  signal() noexcept = default;

  signal(const signal&) = delete;
  signal& operator=(const signal&) = delete;

  signal(signal&& other) noexcept
      : signal{} {
    operator=(std::move(other));
  }

  signal& operator=(signal&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    _connections = std::move(other._connections);
    _token = std::exchange(other._token, nullptr);
    if (_token) {
      if (_token->cur == other._connections.end()) {
        _token->cur = _connections.end();
      }
      _token->sig = this;
    }
    for (auto& connection : _connections) {
      connection._sig = this;
    }
    return *this;
  }

  class connection : public intrusive::list_element<signal_tag> {
    friend class signal;

  public:
    connection() noexcept = default;

    connection(const connection&) = delete;
    connection& operator=(const connection&) = delete;

    connection(connection&& other) noexcept
        : connection{} {
      operator=(std::move(other));
    }

    connection& operator=(connection&& other) noexcept {
      if (this == &other) {
        return *this;
      }
      disconnect();
      _sig = other._sig;
      _slot = std::move(other._slot);
      if (_sig) {
        _sig->_connections.insert(std::next(as_iterator(&other)), *this);
        other.disconnect();
      }
      return *this;
    }

    void disconnect() noexcept {
      if (_sig) {
        for (auto* it = _sig->_token; it; it = it->prev) {
          if (it->cur == as_iterator(this)) {
            ++(it->cur);
          }
        }

        _sig->_connections.erase(as_iterator(this));

        _sig = nullptr;
        _slot = {};
      }
    }

    ~connection() {
      disconnect();
    }

  private:
    explicit connection(signal* sig, slot slot) noexcept
        : _sig{sig}
        , _slot{std::move(slot)} {
      _sig->_connections.push_back(*this);
    }

    signal* _sig{nullptr};
    slot _slot{};
  };

  ~signal() {
    if (_token) {
      _token->sig = nullptr;
    }
    for (auto& connection : _connections) {
      connection._sig = nullptr;
      connection._slot = {};
    }
  }

  connection connect(slot slot) noexcept {
    return connection{this, std::move(slot)};
  }

  void operator()(Args... args) const {
    iterator_token token{this};
    while (token.sig && token.cur != token.sig->_connections.end()) {
      auto current = (token.cur)++;
      current->_slot(args...);
    }
  }

private:
  struct iterator_token {
    explicit iterator_token(const signal* sig)
        : sig{sig}
        , cur{sig->_connections.begin()}
        , prev{sig->_token} {
      sig->_token = this;
    }

    ~iterator_token() {
      if (prev) {
        if (prev->cur == prev->sig->_connections.end()) {
          prev->cur = sig->_connections.end();
        }
        prev->sig = sig;
      }
      if (sig) {
        sig->_token = prev;
      }
    }

    const signal* sig;
    typename connection_list::const_iterator cur;
    iterator_token* prev;
  };

  static list_const_iterator as_iterator(connection* connection) {
    return list_const_iterator(static_cast<intrusive::list_element<signal_tag>*>(connection));
  }

  connection_list _connections;
  mutable iterator_token* _token = nullptr;
};

} // namespace signals
