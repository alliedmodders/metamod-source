#pragma once

#include <string>
#include <cstdint>

#include <tier1/convar.h>

template<typename T = const char*>
class MetamodSourceConVar;

enum EMetaConVarType : int16_t
{
	EMetaConVarType_Invalid = -1,
	EMetaConVarType_Bool,
	EMetaConVarType_Int16,
	EMetaConVarType_UInt16,
	EMetaConVarType_Int32,
	EMetaConVarType_UInt32,
	EMetaConVarType_Int64,
	EMetaConVarType_UInt64,
	EMetaConVarType_Float32,
	EMetaConVarType_Float64,
	EMetaConVarType_String,
	EMetaConVarType_Color,
	EMetaConVarType_Vector2,
	EMetaConVarType_Vector3,
	EMetaConVarType_Vector4,
	EMetaConVarType_Qangle,
	EMetaConVarType_MAX
};

template<typename T>
constexpr EMetaConVarType TranslateMetaConVarType();

struct MetamodConVarCreation_t
{
	std::string m_name;
	std::string m_help;
	int32_t m_flags;

	EMetaConVarType m_type;

	template<typename T>
	T& DefaultValue()	{ return *reinterpret_cast<T*>(m_defaultValue); }
	template<typename T>
	T& MinValue()		{ return *reinterpret_cast<T*>(m_minValue); }
	template<typename T>
	T& MaxValue()		{ return *reinterpret_cast<T*>(m_maxValue); }

	uint8_t m_defaultValue[sizeof(CVValue_t)];
	uint8_t m_minValue[sizeof(CVValue_t)];
	uint8_t m_maxValue[sizeof(CVValue_t)];

	bool m_hasDefault;
	bool m_hasMin;
	bool m_hasMax;

	void* m_changeCallback;
	void** m_conVar;

	template<typename T>
	MetamodConVarCreation_t(const char* name, int32_t flags, const char* help, const T& value, void* cb = nullptr) :
	m_name(name),
	m_help(help),
	m_flags(flags),
	m_type(TranslateMetaConVarType<T>()),
	m_hasDefault(true),
	m_changeCallback(cb)
	{
		DefaultValue<T>() = value;
	}

	template<typename T>
	MetamodConVarCreation_t(const char* name, int32_t flags, const char* help, const T& value, bool min, const T& minValue, bool max, const T& maxValue, void* cb = nullptr) :
	m_name(name),
	m_help(help),
	m_flags(flags),
	m_type(TranslateMetaConVarType<T>()),
	m_hasDefault(true),
	m_hasMin(min),
	m_hasMax(max),
	m_changeCallback(cb)
	{
		DefaultValue<T>() = value;
		MinValue<T>() = minValue;
		MaxValue<T>() = maxValue;
	}
};

// For backwards compatibility
template<typename T>
class MetamodSourceConVar
{
private:
#if defined META_IS_SOURCE2
	using FnConVarChangeCallback_t = void(*)(ConVar<T>* ref, const CSplitScreenSlot nSlot, const T* pNewValue, const T* pOldValue);
	ConVar<T>* m_ConVar;
#else
	using FnConVarChangeCallback_t = void(*)(IConVar* var, const char* pOldValue, float flOldValue);
	ConVar* m_ConVar;
#endif

public:

	MetamodSourceConVar(const char* name, int32_t flags, const char* help, const T& value, FnConVarChangeCallback_t cb = nullptr);
	MetamodSourceConVar(const char* name, int32_t flags, const char* help, const T& value, bool min, const T& minValue, bool max, const T& maxValue, FnConVarChangeCallback_t cb = nullptr);

	inline const T  GetValue( ) const;
	inline const T	GetDefaultValue( ) const;
	inline const T	GetMinValue( ) const;
	inline const T	GetMaxValue( ) const;

	inline void SetDefaultValue( const T& value );
	inline void SetMinValue( const T& value );
	inline void SetMaxValue( const T& value );

	inline void RemoveDefaultValue( );
	inline void RemoveMinValue( );
	inline void RemoveMaxValue( );
	
	inline void GetStringValue( char* dst, size_t len, int index = 0 ) const;
	inline void GetStringDefaultValue( char* dst, size_t len ) const;
	inline void GetStringMinValue( char* dst, size_t len ) const;
	inline void GetStringMaxValue( char* dst, size_t len ) const;

	int yes[offset_of<MetamodSourceConVar<T>, &MetamodSourceConVar<T>::yes>()];
};

#include "metamod_convar.hxx"