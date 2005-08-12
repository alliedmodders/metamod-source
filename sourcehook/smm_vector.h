/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_SMMVECTOR_H
#define _INCLUDE_SMMVECTOR_H

template <class T>
class DynVector
{
public:
	DynVector()
	{
		m_Data = NULL;
		m_CurrentSize = 0;
		m_UsedSize = 0;
	}
	~DynVector()
	{
		clear();
	}
	void clear()
	{
		if (m_Data)
		{
			delete [] m_Data;
			m_Data = NULL;
		}
		m_UsedSize = 0;
		m_CurrentSize = 0;
	}
	void resize(size_t size)
	{
		if (!size)
			clear();
		else
			Grow(size);
		m_UsedSize = size;
	}
	DynVector & operator =(DynVector &src)
	{
		Grow(src.m_UsedSize + 1);
		for (size_t i=0; i<src.m_UsedSize; i++)
			m_Data[i] = src.m_Data[i];
		m_UsedSize = src.m_UsedSize;
		return *this;
	}
	void push_back(const T & t)
	{
		GrowIfNeeded();
		m_Data[m_UsedSize-1] = t;
	}
	size_t size()
	{
		return m_UsedSize;
	}
	T & at(size_t i)
	{
		return m_Data[i];
	}
	T & operator [](size_t i)
	{
		return at(i);
	}
private:
	void Grow(size_t size)
	{
		T * data = new T[size];
		if (m_Data)
		{
			size_t copy = (m_UsedSize > size) ? size : m_UsedSize;
			for (size_t i=0; i<copy; i++)
				data[i] = m_Data[i];
			delete [] m_Data;
		}
		m_Data = data;
		m_CurrentSize = size;
	}
	void GrowIfNeeded()
	{
		if (m_UsedSize + 1 > m_CurrentSize)
			Grow((m_UsedSize+1) * 2);
		m_UsedSize++;
	}
private:
	T * m_Data;
	size_t m_CurrentSize;
	size_t m_UsedSize;
};

#endif //_INCLUDE_SMMVECTOR_H
