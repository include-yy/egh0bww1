// A simple JSON-RPC 2.0 implementation for C++ on Windows, using nlohmann::json
// for JSON handling.  See https://www.jsonrpc.org/specification for details on
// the protocol.  See https://github.com/nlohmann/json for the JSON library.

#pragma once

#include <functional>
#include <iostream>
#include <map>
#include <mutex>
#include <optional>
#include <queue>
#include <string>
#include <syncstream>
#include <thread>
#include <variant>

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

#include "json.hpp"

namespace jsonrpc {

using json = nlohmann::json;

struct Error {
    int code = 0;
    std::string message;
    json data = nullptr;

    friend void to_json(json& j, const Error& e) {
        j = json{ {"code", e.code}, {"message", e.message} };
        if (!e.data.is_null()) j["data"] = e.data;
    }

    friend void from_json(const json& j, Error& e) {
        j.at("code").get_to(e.code);
        j.at("message").get_to(e.message);
        if (j.contains("data")) e.data = j.at("data");
    }
};
// In JSON-RPC spec, id can be string, number, or null.
// For simplicity, we use int here.
struct Request {
    std::optional<int> id;
    std::string method;
    json params = nullptr;

    friend void to_json(json& j, const Request& r) {
        j = json{ {"jsonrpc", "2.0"}, {"method", r.method} };
        if (r.id.has_value()) j["id"] = r.id.value();
        if (!r.params.is_null()) j["params"] = r.params;
    }

    friend void from_json(const json& j, Request& r) {
        if (j.contains("id")) {
            const auto& id_val = j["id"];
            if (id_val.is_number_integer()) {
                r.id = id_val.get<int>();
            } else {
                throw std::runtime_error("Invalid JSON-RPC: Request id must be an integer");
            }
        } else {
            r.id = std::nullopt;
        }
        j.at("method").get_to(r.method);
        if (j.contains("params")) {
            const auto& p = j["params"];
            if (!p.is_structured() && !p.is_null()) {
                throw std::runtime_error("Invalid JSON-RPC: params must be an array or object");
            }
            r.params = p;
        } else {
            r.params = nullptr;
        }
    }
};

struct Response {
    int id = 0;
    std::variant<json, Error> content;

    bool is_error() const { return std::holds_alternative<Error>(content); }

    static Response make_success(int id, json result) {
        return Response{ id, std::move(result) };
    }

    static Response make_error(int id, int code, std::string msg, json data = nullptr) {
        return Response{ id, Error{code, std::move(msg), std::move(data)} };
    }

    friend void to_json(json& j, const Response& r) {
        j = json{ {"jsonrpc", "2.0"}, {"id", r.id} };
        if (std::holds_alternative<Error>(r.content)) {
            j["error"] = std::get<Error>(r.content);
        } else {
            j["result"] = std::get<json>(r.content);
        }
    }

    friend void from_json(const json& j, Response& r) {
        if (!j.contains("id")) {
            throw std::runtime_error("Invalid JSON-RPC: Response must have an id");
        }
        const auto& id_val = j["id"];

        if (id_val.is_number_integer()) {
            r.id = id_val.get<int>();
        } else {
            throw std::runtime_error("Invalid JSON-RPC: Response id must be an integer");
        }

        bool has_result = j.contains("result");
        bool has_error = j.contains("error");

        if (has_result && has_error) {
            throw std::runtime_error("Invalid JSON-RPC: Response cannot have both result and error");
        }
        if (!has_result && !has_error) {
            throw std::runtime_error("Invalid JSON-RPC: Response missing result or error");
        }

        if (has_error) {
            r.content = j.at("error").get<Error>();
        } else {
            r.content = j.at("result");
        }
    }
};

using IncomingMessage = std::variant<Request, Response, Error>;

// JSON parser for incoming messages.
// It can throw exceptions if the JSON is invalid
// or doesn't conform to the expected structure.
struct Parser {
    static IncomingMessage parse(const json& j) {
        if (!j.is_object())
            throw std::runtime_error("Invalid JSON-RPC: not an object");

        if (j.contains("method")) {
            return j.get<Request>();
        }
        if (j.contains("id")) {
            const auto& id_val = j["id"];
            if (id_val.is_number_integer()) {
                return j.get<Response>();
            }
            if (id_val.is_null()) {
                if (j.contains("error")) {
                    return j["error"].get<Error>();
                } else {
                    throw std::runtime_error("Invalid JSON-RPC: id is null but no error object");
                }
            }
            throw std::runtime_error("Invalid JSON-RPC: id must be integer or null");
        }
        throw std::runtime_error("Invalid JSON-RPC: Broken message");
    }
    // Firstly, parse the string into JSON.
    static IncomingMessage parse(const std::string& str) {
        return parse(json::parse(str));
    }
};
}  // namespace jsonrpc

// Code for stdio-based JSON-RPC connection handling on Windows.
// Specifically for Win32 Message Loop integration.  It reads from
// stdin, writes to stdout, and uses PostThreadMessage to notify
// the main thread of new messages.
namespace jsonrpc {

// Thread-safe queue for incoming messages.
template <typename T>
class ThreadSafeQueue {
private:
    std::queue<T> queue_;
    std::mutex mutex_;

public:
    void push(T value) {
        std::lock_guard<std::mutex> lock(mutex_);
        queue_.push(std::move(value));
    }
    bool try_pop(T& value) {
        std::lock_guard<std::mutex> lock(mutex_);
        if (queue_.empty()) return false;
        value = std::move(queue_.front());
        queue_.pop();
        return true;
    }
};
// Forward declaration of Conn for Context to use.
class Conn;
// Context passed to async request handlers, allowing them to reply or send
// errors.
class Context {
public:
    Context(Conn& c, std::optional<int> i) : conn_(c), id_(i) {}

    void reply(json result);
    void error(int code, std::string message, json data = nullptr);
    std::optional<int> id() const { return id_; }
    bool is_notification() const { return !id_.has_value(); }

private:
    Conn& conn_;
    std::optional<int> id_;
};

// The main connection class that manages the JSON-RPC communication.
// This name comes from Emacs's jsonrpc.el, which uses jsonrpc-connection
// as the basic class for handling JSON-RPC connections.
class Conn {
public:
    using AsyncRequestHandler = std::function<void(Context, const json&)>;
    using RequestHandler = std::function<json(const json&)>;
    using ResponseHandler = std::function<void(const Response&)>;
    using NotificationHandler = std::function<void(const json&)>;
    using RawHandler = std::function<bool(const IncomingMessage&, Conn&)>;
    using Waker = std::function<void()>;

    Conn(Waker waker) : next_id_(1), running_(false), waker_(std::move(waker)) {
        // Force Windows stdin/stdout into binary mode to prevent \r\n
        // translation that can cause Content-Length miscalculation or 
        // byte reading errors.
#ifdef _WIN32
        (void)_setmode(_fileno(stdin), _O_BINARY);
        (void)_setmode(_fileno(stdout), _O_BINARY);
#endif
    }

    ~Conn() { stop(); }

    void set_raw_handler(RawHandler handler) {
        raw_handler_ = std::move(handler);
    }

    void start() {
        if (running_) return;
        running_ = true;
        // Start the reader thread that continuously reads from stdin
        // and pushes messages to the inbox queue.
        reader_thread_ = std::thread([this]() { read_loop(); });
    }

    void stop() {
        running_ = false;
        if (reader_thread_.joinable()) reader_thread_.detach();
    }
    // Register an async method handler for incoming requests. The handler
    // receives a Context object.
    void register_async_method(const std::string& name,
        AsyncRequestHandler handler) {
        std::lock_guard<std::mutex> lock(map_mutex_);
        method_handlers_[name] = handler;
    }
    // Register a sync method handler for incoming requests. The handler returns a
    // json result directly.
    void register_method(const std::string& name, RequestHandler handler) {
        // Wrap Sync into Async automatically
        register_async_method(name, [handler](Context ctx, const json& params) {
            try {
                // Call user logic, get result, reply immediately
                json res = handler(params);
                ctx.reply(std::move(res));
            } catch (const std::exception& e) {
                ctx.error(-32603, e.what());
            }
            });
    }
    // Register a notification handler.
    void register_notification(const std::string& name,
        NotificationHandler handler) {
        register_async_method(name, [handler](Context ctx, const json& params) {
            if (ctx.is_notification()) {
                handler(params);
            } else {
                ctx.error(-32600, "Notification handler expects no id");
            }
            });
    }

    // Send a request to the other side, with a callback for the response.
    void send_request(const std::string& method, const json& params,
        ResponseHandler callback) {
        int id = next_id_++;
        {
            std::lock_guard<std::mutex> lock(map_mutex_);
            pending_callbacks_[id] = callback;
        }
        Request req{ id, method, params };
        json j;
        to_json(j, req);
        send_message(j.dump());
    }
    // Send a notification to other side.
    void send_notification(const std::string& method,
        const json& params = nullptr) {
        Request req{ std::nullopt, method, params };
        json j;
        to_json(j, req);
        send_message(j.dump());
    }
    // Stdin Read Loop
    void process_queue() {
        IncomingMessage msg;
        while (inbox_.try_pop(msg)) {

            if (raw_handler_) {
                if (raw_handler_(msg, *this)) {
                    continue;
                }
            }

            if (std::holds_alternative<Request>(msg)) {
                handle_request(std::get<Request>(msg));
            } else if (std::holds_alternative<Response>(msg)) {
                handle_response(std::get<Response>(msg));
            } else if (std::holds_alternative<Error>(msg)) {
                const auto& err = std::get<Error>(msg);
                std::cerr << "[JSON-RPC FATAL ERROR] "
                    << "Code: " << err.code
                    << ", Message: " << err.message << std::endl;
                if (!err.data.is_null()) {
                    std::cerr << "Data: " << err.data.dump() << std::endl;
                }
            }
        }
    }
    // Low-level send (Used by Context)
    void send_response_success(int id, json result) {
        json j;
        to_json(j, Response::make_success(id, std::move(result)));
        send_message(j.dump());
    }
    void send_response_error(int id, int code, std::string msg, json data) {
        json j;
        to_json(j, Response::make_error(id, code, std::move(msg), std::move(data)));
        send_message(j.dump());
    }

private:
    std::atomic<bool> running_;
    std::atomic<int> next_id_;
    std::thread reader_thread_;
    Waker waker_;

    ThreadSafeQueue<IncomingMessage> inbox_;

    std::mutex map_mutex_;
    RawHandler raw_handler_;
    std::map<std::string, AsyncRequestHandler> method_handlers_;
    std::map<int, ResponseHandler> pending_callbacks_;

    void send_message(const std::string& body) {
        // std::osyncstream will atomically write the buffer to the stream
        // when it is destructed, so we don't need to manually lock.

        // Emacs's jsonrpc use a HTTP-like framing with Content-Length header,
        // so we follow the same format here:
        // Content-Length: <length>\r\n\r\n<body>
        std::osyncstream(std::cout) << "Content-Length: " << body.length() << "\r\n"
            << "\r\n"
            << body << std::flush;
    }
    void send_response_error_raw(json id, int code, std::string msg, json data) {
        json j;
        j = json{
            {"jsonrpc", "2.0"},
            {"id", id},
            {"error", {{"code", code}, {"message", msg}}}
        };
        if (!data.is_null()) j["error"]["data"] = data;
        send_message(j.dump());
    }
    void read_loop() {
        // A helper function that reads a line and automatically removes the Windows
        // \r character.
        auto read_header_line = []() -> std::optional<std::string> {
            std::string line;
            if (!std::getline(std::cin, line))
                return std::nullopt;  // EOF or stream error
            if (!line.empty() && line.back() == '\r') line.pop_back();
            return line;
            };

        // If we can ensure that input always is valid JSON and well-formed,
        // we can simplify the reading logic by just reading the Content-Length
        // header and then reading the exact number of bytes for the body. Thus,
        // -32700 and -32600 error handling is not needed.
        while (running_) {
            int content_length = 0;

            // Read header.
            while (true) {
                auto line = read_header_line();
                if (!line) return;  // stream error or EOF, exit the loop
                if (line->empty())
                    break;  // empty line indicates end of headers, ready to read body

                if (line->starts_with("Content-Length: ")) {
                    // "Content-Length: " is 16 characters long.
                    try {
                        content_length = std::stoi(line->substr(16));
                    } catch (...) {
                    }
                }
            }
            // invalid content length, skip to next message
            if (content_length <= 0) continue;

            // Read whole body.
            std::vector<char> buffer(content_length);
            std::cin.read(buffer.data(), content_length);

            // Make sure we read the exact number of bytes specified by
            // Content-Length. If not, it means the stream is broken or we reached
            // EOF, so we should exit the loop.
            if (std::cin.gcount() != content_length) break;

            // Parse and post message to main thread
            try {
                // Gemini says below is faster.
                // auto msg = Parser::parse(std::string(buffer.begin(), buffer.end()));
                auto j = json::parse(buffer.begin(), buffer.end());
                auto msg = Parser::parse(j);
                inbox_.push(std::move(msg));

                if (waker_) {
                    waker_();
                }
            } catch (const json::parse_error&) {
                send_response_error_raw(json(nullptr), -32700, "Parse error", nullptr);
            } catch (const std::exception& e) {
                send_response_error_raw(json(nullptr), -32600, "Invalid Request", e.what());
            }
        }
    }

    void handle_request(const Request& req) {
        AsyncRequestHandler handler = nullptr;
        {
            std::lock_guard<std::mutex> lock(map_mutex_);
            if (method_handlers_.count(req.method)) {
                handler = method_handlers_[req.method];
            }
        }

        if (handler) {
            // Pass Context to handler. It is responsible for replying.
            try {
                handler(Context(*this, req.id), req.params);
            } catch (const std::exception& e) {
                // TODO: Consider -32602 Invalid params error if the handler
                // throws an exception related to parameter parsing or validation.
                if (req.id.has_value()) {
                    send_response_error(req.id.value(), -32603, e.what(), nullptr);
                }
            }
        } else {
            if (req.id.has_value()) {
                send_response_error(req.id.value(), -32601, "Method not found",
                    nullptr);
            }
        }
    }

    void handle_response(const Response& resp) {
        ResponseHandler callback = nullptr;
        {
            std::lock_guard<std::mutex> lock(map_mutex_);
            auto it = pending_callbacks_.find(resp.id);
            if (it != pending_callbacks_.end()) {
                callback = std::move(it->second);
                pending_callbacks_.erase(it);
            }
        }
        if (callback) callback(resp);
    }
};
// Implement Context methods inline after Conn is defined.
// Send a success result (can be called from any thread)
inline void Context::reply(json result) {
    if (id_.has_value()) {
        conn_.send_response_success(id_.value(), std::move(result));
    }
}
// Send an error (can be called from any thread)
inline void Context::error(int code, std::string message, json data) {
    if (id_.has_value()) {
        conn_.send_response_error(id_.value(), code, std::move(message),
            std::move(data));
    }
}
}  // namespace jsonrpc
