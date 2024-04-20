#pragma once

#include "sourcehook_impl.h"
#include "sourcehook_hookmangen.h"

#include <vector>
#include <string>
#include <ISmmPluginExt.h>

extern SourceHook::ISourceHook* g_SHPtr;
extern SourceMM::PluginId g_PLID;

namespace SourceHook {
namespace Impl {
void run_tests();

class SDKVector
{
public:
	SDKVector(float x1, float y1, float z1)
	{
		this->x = x1;
		this->y = y1;
		this->z = z1;
	}
	SDKVector(void)
	{
		this->x = 0.0;
		this->y = 0.0;
		this->z = 0.0;
	}
	float x;
	float y;
	float z;
};

class SHTException : public std::exception {
public:
	SHTException(const char* message) : std::exception(message) {};
};

class SHT {
public:
	SHT(const char* name, void (*func)(void)) noexcept : m_function(func), m_name(name) {
		m_tests.push_back(this);
	};

	void Call() { m_function(); }
	const std::string& GetName() { return m_name; }

	static std::vector<SHT*> m_tests;
private:
	void (*m_function)(void);
	std::string m_name;
};
}
}