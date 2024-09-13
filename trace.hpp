#pragma once

#include <fstream>
#include <string>
#include <ctime>
#include <locale>
#include <codecvt>
#include <iomanip>
#include <iostream>
#include <chrono>

namespace debugtrace {

template<typename T>
inline T wconvert(T&& x)
	{ return std::move(x); }
template<typename T, typename R>
inline R wconvert(T&& x)
	{ return R(std::move(x)); }
template<>
inline std::wstring wconvert<std::string, std::wstring>(std::string&& x)
	{ return std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>>().from_bytes(x); }
template<>
inline std::string wconvert<std::wstring, std::string>(std::wstring&& x)
	{ return std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>>().to_bytes(x); }

struct now {};

template<typename C>
std::basic_ostream<C>& operator <<(std::basic_ostream<C>& output, const struct now& _) {
	std::time_t t = std::time(nullptr);
	auto fmt = wconvert<std::string, std::basic_string<C>>("[%H:%M:%S.");
	auto time = std::put_time<C>(std::localtime(&t), fmt.c_str());
	output << time
		<< std::setw(3) << std::setfill(wconvert<char, C>('0'))
		<< std::chrono::duration_cast<std::chrono::milliseconds>
			(std::chrono::system_clock::now().time_since_epoch()).count() % 1000
		<< ']';
	return output;
}

template<typename T>
struct nospace: public T { using T::T; };

struct noendl {};

#if defined(DEBUGTRACE_OUTPUT_FILE)
	#if defined(DEBUGTRACE_WIDE)
		inline std::wofstream output() { return std::wofstream(DEBUGTRACE_OUTPUT_FILE, std::ios::app); }
	#else
		inline std::ofstream output() { return std::ofstream(DEBUGTRACE_OUTPUT_FILE, std::ios::app); }
	#endif
#elif defined(DEBUGTRACE_OUTPUT_STREAM)
	#if defined(DEBUGTRACE_WIDE)
		inline std::basic_ostream<wchar_t>& output() { return DEBUGTRACE_OUTPUT_STREAM; }
	#else
		inline std::basic_ostream<char>& output() { return DEBUGTRACE_OUTPUT_STREAM; }
	#endif
#else
	#if defined(DEBUGTRACE_WIDE)
		inline std::wostream& output() { return std::wcerr; }
	#else
		inline std::ostream& output() { return std::cerr; }
	#endif
#endif

#ifdef NDEBUG
	#define trace_to_single(out, x)
#else
	#define trace_to_single(out, x) { out << x; }
#endif

template<typename S, typename X>
X& trace_to(S&& out, X& x, const noendl& _)
	{ trace_to_single(out, x); return x; }
template<typename S, typename X>
X& trace_to(S&& out, X& x)
	{ trace_to_single(out, x); trace_to_single(out, std::endl); return x; }
template<typename S, typename X>
X&& trace_to(S&& out, X&& x)
	{ trace_to(out, x); return std::move(x); }

template<typename S, typename M, typename X, typename ...Xs>
decltype(auto) trace_to(S&& out, nospace<M> x, Xs&& ...xs) {
	trace_to_single(out, dynamic_cast<M>(x));
	return trace_to(out, xs...);
}

template<typename S, typename X, typename ...Xs>
decltype(auto) trace_to(S&& out, X&& x, Xs&& ...xs) {
	trace_to_single(out, x);
	trace_to_single(out, ' ');
	return trace_to(out, xs...);
}

template<typename ...Xs>
decltype(auto) trace(Xs&& ...xs) {
	return trace_to(output(), xs...);
}

template<typename S, typename F, typename X>
decltype(auto) trace_with_to(S&& out, F&& f, X&& x) {
	trace_to(std::move(out), f(x));
	return std::forward(x);
}

template<typename F, typename X>
decltype(auto) trace_with(F&& f, X&& x) {
	return trace_with_to(output(), f, x);
}

#undef trace_to_single

}
