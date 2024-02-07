#pragma once

template<> constexpr EMetaConVarType TranslateMetaConVarType<bool>( void )		{ return EMetaConVarType_Bool; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<int16_t>( void )	{ return EMetaConVarType_Int16; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<uint16_t>( void )	{ return EMetaConVarType_UInt16; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<int32_t>( void )	{ return EMetaConVarType_Int32; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<uint32_t>( void )	{ return EMetaConVarType_UInt32; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<int64_t>( void )	{ return EMetaConVarType_Int64; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<uint64_t>( void )	{ return EMetaConVarType_UInt64; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<float>( void )		{ return EMetaConVarType_Float32; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<double>( void )	{ return EMetaConVarType_Float64; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<const char*>( void ){ return EMetaConVarType_String; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<Color>( void )		{ return EMetaConVarType_Color; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<Vector2D>( void )	{ return EMetaConVarType_Vector2; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<Vector>( void )	{ return EMetaConVarType_Vector3; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<Vector4D>( void )	{ return EMetaConVarType_Vector4; }
template<> constexpr EMetaConVarType TranslateMetaConVarType<QAngle>( void )	{ return EMetaConVarType_Qangle; }

template<typename T>
MetamodSourceConVar<T>::MetamodSourceConVar(const char* name, int32_t flags, const char* help, const T& value, FnConVarChangeCallback_t cb) :
m_ConVar(nullptr)
{
	MetamodConVarCreation_t create(name, flags, help, value, reinterpret_cast<void*>(cb));
}

template<typename T>
MetamodSourceConVar<T>::MetamodSourceConVar(const char* name, int32_t flags, const char* help, const T& value, bool min, const T& minValue, bool max, const T& maxValue, FnConVarChangeCallback_t cb) :
m_ConVar(nullptr)
{
	MetamodConVarCreation_t create(name, flags, help, value, cb, min, minValue, max, maxValue, reinterpret_cast<void*>(cb));
}

#if defined META_IS_SOURCE2
template<typename T>
inline const T MetamodSourceConVar<T>::GetValue() const
{
	return m_ConVar->GetValue();
}

template<typename T>
inline const T	MetamodSourceConVar<T>::GetDefaultValue( ) const
{
	return m_ConVar->GetDefaultValue( );
}

template<typename T>
inline const T	MetamodSourceConVar<T>::GetMinValue( ) const
{
	return m_ConVar->GetMinValue( );
}

template<typename T>
inline const T	MetamodSourceConVar<T>::GetMaxValue( ) const
{
	return m_ConVar->GetMaxValue( );
}

template<typename T>
inline void MetamodSourceConVar<T>::SetDefaultValue( const T& value )
{
	m_ConVar->SetDefaultValue( value );
}

template<typename T>
inline void MetamodSourceConVar<T>::SetMinValue( const T& value )
{
	m_ConVar->SetMinValue( value );
}

template<typename T>
inline void MetamodSourceConVar<T>::SetMaxValue( const T& value )
{
	m_ConVar->SetMaxValue( value );
}

template<typename T>
inline void MetamodSourceConVar<T>::RemoveDefaultValue( )
{
	m_ConVar->RemoveDefaultValue( );
}

template<typename T>
inline void MetamodSourceConVar<T>::RemoveMinValue( )
{
	m_ConVar->RemoveMinValue( );
}

template<typename T>
inline void MetamodSourceConVar<T>::RemoveMaxValue( )
{
	m_ConVar->RemoveMaxValue( );
}

template<typename T>
inline void MetamodSourceConVar<T>::GetStringValue( char* dst, size_t len, int index ) const
{
	m_ConVar->GetStringValue( dst, len, index );
}

template<typename T>
inline void MetamodSourceConVar<T>::GetStringDefaultValue( char* dst, size_t len ) const
{
	m_ConVar->GetStringDefaultValue( dst, len );
}

template<typename T>
inline void MetamodSourceConVar<T>::GetStringMinValue( char* dst, size_t len ) const
{
	m_ConVar->GetStringMinValue( dst, len );
}

template<typename T>
inline void MetamodSourceConVar<T>::GetStringMaxValue( char* dst, size_t len ) const
{
	m_ConVar->GetStringMaxValue( dst, len );
}
#else



#endif