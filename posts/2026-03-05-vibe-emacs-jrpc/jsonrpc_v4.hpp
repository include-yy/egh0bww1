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
#endif

#include "json.hpp"

namespace jsonrpc {

using json = nlohmann::json;

// Namespace for JSON-RPC 2.0 specifications and constants.
namespace spec {

/**
 *  __________________________________________________________________________________
 * | Code   | Message          | Meaning                                              |
 * |________|__________________|______________________________________________________|
 * |-32700  | Parse error      | Invalid JSON received (parsing failed).              |
 * |-32600  | Invalid Request  | The JSON sent is not a valid Request object.         |
 * |-32601  | Method not found | The method does not exist / is not available.        |
 * |-32602  | Invalid params   | Invalid method parameter(s).                         |
 * |-32603  | Internal error   | Internal JSON-RPC error.                             |
 * |-320xx  | Server error     | -32000 to -32099 reserved for implementation errors. |
 * |________|__________________|______________________________________________________|
 */

constexpr int kParseError = -32700;
constexpr int kInvalidRequest = -32600;
constexpr int kMethodNotFound = -32601;
constexpr int kInvalidParams = -32602;
constexpr int kInternalError = -32603;

constexpr const char* msg_ParseError = "Parse Error";
constexpr const char* msg_InvalidRequest = "Invalid Request";
constexpr const char* msg_MethodNotFound = "Method not found";
constexpr const char* msg_InvalidParams = "Invalid params";
constexpr const char* msg_InternalError = "Internal error";

// Detailed error messages for internal validation usage.
namespace details {

constexpr const char* err_code_miss = "Invalid JSON-RPC: Error object must have a 'code'";
constexpr const char* err_code_type = "Invalid JSON-RPC: Error 'code' must be an integer";
constexpr const char* err_msg_miss = "Invalid JSON-RPC: Error object must have a 'message'";
constexpr const char* err_msg_type = "Invalid JSON-RPC: Error 'message' must be a string";

constexpr const char* req_id_type = "Invalid JSON-RPC: Request id must be an integer";
constexpr const char* req_params_type = "Invalid JSON-RPC: params must be an array or object";
constexpr const char* req_method_miss = "Invalid JSON-RPC: Request must have a method string";

constexpr const char* resp_missing_id = "Invalid JSON-RPC: Response must have an id";
constexpr const char* resp_id_type = "Invalid JSON-RPC: Response id must be an integer";
constexpr const char* resp_conflict = "Invalid JSON-RPC: Response cannot have both result and error";
constexpr const char* resp_incomplete = "Invalid JSON-RPC: Response missing result or error";

constexpr const char* parse_not_object = "Invalid JSON-RPC: not an object";
constexpr const char* parse_id_null_no_error = "Invalid JSON-RPC: id is null but no error object";
constexpr const char* parse_id_type = "Invalid JSON-RPC: id must be integer or null";
constexpr const char* parse_broken = "Invalid JSON-RPC: Message must have method or id";

} // namespace details

} // namespace spec

// Custom exception class for JSON-RPC specific errors.
// Carrying the error code allows the Connection layer to send back
// the correct error response automatically.
struct JsonRpcException : public std::runtime_error {
    int code;
    json data;

    static const char* code_to_message(int c) {
        switch (c) {
        case spec::kParseError:     return spec::msg_ParseError;
        case spec::kInvalidRequest: return spec::msg_InvalidRequest;
        case spec::kMethodNotFound: return spec::msg_MethodNotFound;
        case spec::kInvalidParams:  return spec::msg_InvalidParams;
        default:                    return spec::msg_InternalError;
        }
    }

    // Constructor with custom message.
    JsonRpcException(int c, const char* msg, json d = nullptr)
        : std::runtime_error(msg), code(c), data(std::move(d)) {}
    // Constructor using standard message based on code.
    explicit JsonRpcException(int c, json d = nullptr)
        : std::runtime_error(code_to_message(c)), code(c), data(std::move(d)) {}
};

// Represents a JSON-RPC Error object.
// Used inside Response or as a standalone message for null id cases.
struct Error {
    int code = 0;
    std::string message;
    json data = nullptr;

    friend void to_json(json& j, const Error& e) {
        j = json{ {"code", e.code}, {"message", e.message} };
        if (!e.data.is_null()) {
            j["data"] = e.data;
        }
    }

    friend void from_json(const json& j, Error& e) {
        if (!j.contains("code")) {
            throw JsonRpcException(spec::kInternalError, spec::details::err_code_miss);
        }
        const auto& code_val = j["code"];
        if (!code_val.is_number_integer()) {
            throw JsonRpcException(spec::kInternalError, spec::details::err_code_type);
        }
        e.code = code_val.get<int>();
        if (!j.contains("message")) {
            throw JsonRpcException(spec::kInternalError, spec::details::err_msg_miss);
        }
        const auto& msg_val = j["message"];
        if (!msg_val.is_string()) {
            throw JsonRpcException(spec::kInternalError, spec::details::err_msg_type);
        }
        e.message = msg_val.get<std::string>();
        if (j.contains("data")) {
            e.data = j["data"];
        } else {
            e.data = nullptr;
        }
    }
};

// Represents a JSON-RPC Request object.
// In JSON-RPC spec, id can be string, number, or null.
// For simplicity and strong typing, we strictly enfore int for id.
struct Request {
    std::optional<int> id; // nullopt means it's a Notification.
    std::string method;
    json params = nullptr;

    friend void to_json(json& j, const Request& r) {
        j = json{ {"jsonrpc", "2.0"}, {"method", r.method} };
        if (r.id.has_value()) {
            j["id"] = r.id.value();
        }
        if (!r.params.is_null()) {
            j["params"] = r.params;
        }
    }

    friend void from_json(const json& j, Request& r) {
        // 1. Parse id.
        if (j.contains("id")) {
            const auto& id_val = j["id"];
            if (id_val.is_number_integer()) {
                r.id = id_val.get<int>();
            } else {
                throw JsonRpcException(spec::kInvalidRequest, spec::details::req_id_type);
            }
        } else {
            r.id = std::nullopt;
        }

        // 2. Parse Method.
        if (!j.contains("method") || !j["method"].is_string()) {
            throw JsonRpcException(spec::kInvalidRequest, spec::details::req_method_miss);
        }
        r.method = j["method"].get<std::string>();

        // 3. Parse Params.
        if (j.contains("params")) {
            const auto& p = j["params"];
            if (!p.is_structured() && !p.is_null()) {
                throw JsonRpcException(spec::kInvalidRequest, spec::details::req_params_type);
            }
            r.params = p;
        } else {
            r.params = nullptr;
        }
    }
};

// Represents a JSON-RPC Response object.
// Strictly enforces 'int' id. 'null' id (protocol error) should be handled
// by Parser and converted to Error object, not Response.
struct Response {
    int id = 0;
    std::variant<json, Error> content;

    bool is_error() const {
        return std::holds_alternative<Error>(content);
    }

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
        // 1. ID Validation.
        if (!j.contains("id")) {
            throw JsonRpcException(spec::kInvalidRequest, spec::details::resp_missing_id);
        }
        const auto& id_val = j["id"];

        if (id_val.is_number_integer()) {
            r.id = id_val.get<int>();
        } else {
            // Parser handles "id: null" logic, so here it must be int.
            throw JsonRpcException(spec::kInvalidRequest, spec::details::resp_id_type);
        }
        // 2. Result/Error Mutual Exclusion.
        bool has_result = j.contains("result");
        bool has_error = j.contains("error");

        if (has_result && has_error) {
            throw JsonRpcException(spec::kInvalidRequest, spec::details::resp_conflict);
        }
        if (!has_result && !has_error) {
            throw JsonRpcException(spec::kInvalidRequest, spec::details::resp_incomplete);
        }
        // 3. Content Extraction.
        if (has_error) {
            r.content = j.at("error").get<Error>();
        } else {
            r.content = j.at("result");
        }
    }
};

// A variant capable of holding any valid incoming message type.
using IncomingMessage = std::variant<Request, Response, Error>;

// The Parser acts as a router/validator.
// It determines the type of the message and delegates strict parsing
// to the respective struct's from_json method.
struct Parser {
    static IncomingMessage parse(const json& j) {
        if (!j.is_object()) {
            throw JsonRpcException(spec::kInvalidRequest, spec::details::parse_not_object);
        }
        // Case 1: Request or Notification.
        if (j.contains("method")) {
            return j.get<Request>();
        }
        // Case 2: Response or Protocol Error.
        if (j.contains("id")) {
            const auto& id_val = j["id"];
            // Normal Response
            if (id_val.is_number_integer()) {
                return j.get<Response>();
            }
            // Protocol Error (id is null).
            if (id_val.is_null()) {
                if (j.contains("error")) {
                    return j["error"].get<Error>();
                } else {
                    throw JsonRpcException(spec::kInvalidRequest, spec::details::parse_id_null_no_error);
                }
            }
            throw JsonRpcException(spec::kInvalidRequest, spec::details::parse_id_type);
        }
        throw JsonRpcException(spec::kInvalidRequest, spec::details::parse_broken);
    }
    // Helper: Parse from string.
    static IncomingMessage parse(const std::string& str) {
        return parse(json::parse(str));
    }
};
}  // namespace jsonrpc

// Code for stdio-based JSON-RPC connection handling.
namespace jsonrpc {

// Thread-safe queue for buffering incoming messages from the reader thread.
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

class Conn; // Forward declaration.

// Context passed to async request handlers.
// Allows handlers to reply or send errors asynchronously.
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
// Implements the "LSP-style" Content-Length framing over Stdin/Stdout.
// Typically used to integrate with Emacs's jsonrpc.el.
class Conn {
public:
    using AsyncRequestHandler = std::function<void(Context, const json&)>;
    using RequestHandler = std::function<json(const json&)>;
    using ResponseHandler = std::function<void(const Response&)>;
    using NotificationHandler = std::function<void(const json&)>;
    using RawHandler = std::function<bool(const IncomingMessage&, Conn&)>;
    using Waker = std::function<void()>;

    // Constructor: waker is called whenever a new message arrives in the queue.
    // Use it to wake up your main event loop.
    Conn(Waker waker) : next_id_(1), running_(false), waker_(std::move(waker)) {
        // Force Windows stdin/stdout into binary mode to prevent \r\n translation.
        // Critical for correct Content-Length calculation.
#ifdef _WIN32
        (void)_setmode(_fileno(stdin), _O_BINARY);
        (void)_setmode(_fileno(stdout), _O_BINARY);
#endif
    }

    ~Conn() { stop(); }

    // Start the background reader thread.
    void start() {
        if (running_) return;
        running_ = true;
        // Start the reader thread that continuously reads from stdin
        // and pushes messages to the inbox queue.
        reader_thread_ = std::thread([this]() { read_loop(); });
    }

    // Stop the reader thread and cleanup.
    void stop() {
        running_ = false;
        if (reader_thread_.joinable()) reader_thread_.detach();
    }

    // Set a raw handler to intercept all incoming messages (advanced usage).
    // If the handler returns true, the message is considered handled and won't
    // be processed further.
    void set_raw_handler(RawHandler handler) {
        std::lock_guard<std::mutex> lock(map_mutex_);
        raw_handler_ = std::move(handler);
    }

    // Register an async method.
    void register_async_method(const std::string& name, AsyncRequestHandler handler) {
        std::lock_guard<std::mutex> lock(map_mutex_);
        method_handlers_[name] = handler;
    }

    // Register a sync method.
    // Wraps the sync handler into an async one.
    void register_method(const std::string& name, RequestHandler handler) {
        register_async_method(name, [handler](Context ctx, const json& params) {
            try {
                // Call user logic, get result, reply immediately
                json res = handler(params);
                ctx.reply(std::move(res));
            } catch (const JsonRpcException& e) {
                // Allow handler to throw JsonRpcException directly.
                ctx.error(e.code, e.what(), e.data);
            } catch (const std::exception& e) {
                // Catch generic exceptions as Internal Error.
                ctx.error(spec::kInternalError, e.what());
            }
            });
    }

    // Register a notification handler.
    void register_notification(const std::string& name, NotificationHandler handler) {
        register_async_method(name, [handler](Context ctx, const json& params) {
            if (ctx.is_notification()) {
                handler(params);
            } else {
                ctx.error(spec::kInvalidRequest, "Notification handler expects no id");
            }
            });
    }

    // Send a Request to the other side, with a callback for the response.
    void send_request(const std::string& method, const json& params, ResponseHandler callback) {
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
    void send_notification(const std::string& method, const json& params = nullptr) {
        Request req{ std::nullopt, method, params };
        json j;
        to_json(j, req);
        send_message(j.dump());
    }

    // Public method to reply with success (used by Context).
    void send_response_success(int id, json result) {
        json j;
        to_json(j, Response::make_success(id, std::move(result)));
        send_message(j.dump());
    }
    // Public method to reply with error (used by COntext).
    void send_response_error(int id, int code, std::string msg, json data = nullptr) {
        json j;
        to_json(j, Response::make_error(id, code, std::move(msg), std::move(data)));
        send_message(j.dump());
    }

    // Main Loop Processor: Call this from your main thread/event loop.
    // It processes messages from the inbox queue.
    void process_queue() {
        IncomingMessage msg;
        while (inbox_.try_pop(msg)) {
            // 1. Raw Handler Interception.
            RawHandler handler_copy = nullptr;
            {
                std::lock_guard<std::mutex> lock(map_mutex_);
                handler_copy = raw_handler_;
            }
            if (handler_copy) {
                if (handler_copy(msg, *this)) {
                    continue;
                }
            }
            // 2. Dispatch based on message type.
            if (std::holds_alternative<Request>(msg)) {
                handle_request(std::get<Request>(msg));
            } else if (std::holds_alternative<Response>(msg)) {
                handle_response(std::get<Response>(msg));
            } else if (std::holds_alternative<Error>(msg)) {
                // Handle "Global Error" (id: null), usually a Protocol Error from peer.
                // Log to stderr as it cannot be replied to.
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

    // Thread-safe message sender.
    void send_message(const std::string& body) {
        // std::osyncstream will atomically write the buffer to the stream
        // when it is destructed, so we don't need to manually lock.

        // Emacs's jsonrpc use a HTTP-like framing with Content-Length header,
        // Content-Length: <length>\r\n\r\n<body>
        std::osyncstream(std::cout)
            << "Content-Length: " << body.length() << "\r\n"
            << "\r\n" << body << std::flush;
    }

    // Helper: Send a protocol-level error where id is null.
    // Used when we cannot parse the request or the ID is invalid.
    void send_protocol_error(int code, std::string msg, json data = nullptr) {
        json j = json{
            {"jsonrpc", "2.0"},
            {"id", nullptr}, // Must be null for protocol errors.
            {"error", {{"code", code}, {"message", msg}}}
        };
        if (!data.is_null()) {
            j["error"]["data"] = data;
        }
        send_message(j.dump());
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
            try {
                // Pass Context to handler. It is responsible for replying.
                handler(Context(*this, req.id), req.params);
            } catch (const JsonRpcException& e) {
                if (req.id.has_value()) {
                    send_response_error(req.id.value(), e.code, e.what(), e.data);
                }
            } catch (const std::exception& e) {
                // User threw a generic exception -> Internal Error
                if (req.id.has_value()) {
                    send_response_error(req.id.value(), spec::kInternalError, e.what());
                }
            }
        } else {
            // Method not found.
            if (req.id.has_value()) {
                send_response_error(req.id.value(), spec::kMethodNotFound, spec::msg_MethodNotFound);
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
    void read_loop() {
        // A helper function that reads a line and automatically removes '\r' character.
        auto read_header_line = []() -> std::optional<std::string> {
            std::string line;
            if (!std::getline(std::cin, line)) {
                return std::nullopt;  // EOF or stream error
            }
            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }
            return line;
            };

        while (running_) {
            int content_length = 0;

            // 1. Read header.
            while (true) {
                auto line = read_header_line();
                if (!line) return;  // EOF
                if (line->empty()) break;  // End of headers.

                if (line->starts_with("Content-Length: ")) {
                    // "Content-Length: " length is 16.
                    try {
                        content_length = std::stoi(line->substr(16));
                    } catch (...) {}
                }
            }
            // Invalid content length, skip to next message.
            if (content_length <= 0) continue;

            // 2. Read body.
            std::vector<char> buffer(content_length);
            std::cin.read(buffer.data(), content_length);

            // Make sure we read the exact number of bytes specified.
            if (std::cin.gcount() != content_length) break;

            // 3. Parse and Push.
            try {
                auto j = json::parse(buffer.begin(), buffer.end());
                auto msg = Parser::parse(j);
                inbox_.push(std::move(msg));

                if (waker_) waker_();
            } catch (const json::parse_error&) {
                send_protocol_error(spec::kParseError, spec::msg_ParseError);
            } catch (const JsonRpcException& e) {
                // Catch Invalid Request, etc. from Parser.
                send_protocol_error(e.code, e.what(), e.data);
            } catch (const std::exception& e) {
                // Catch-all for unexpected errors during parsing.
                send_protocol_error(spec::kInvalidRequest, e.what());
            }
        }
    }
};

// Implement Context methods inline after Conn is defined.
inline void Context::reply(json result) {
    if (id_.has_value()) {
        conn_.send_response_success(id_.value(), std::move(result));
    }
}
inline void Context::error(int code, std::string message, json data) {
    if (id_.has_value()) {
        conn_.send_response_error(id_.value(), code, std::move(message), std::move(data));
    }
}
}  // namespace jsonrpc
