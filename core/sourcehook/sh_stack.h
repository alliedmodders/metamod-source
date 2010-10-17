/* ======== SourceMM ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SH_STACK_H__
#define __SH_STACK_H__

#include <stddef.h>
#include "sh_vector.h"

namespace SourceHook
{
	// May _never_ reallocate used memory!

	template <class T> class CStack
	{
		static const size_t SECTOR_SIZE = 16;

		CVector<T*> m_Sectors;				// Stores sectors

		size_t m_UsedSize;

		void clear()
		{
			typename CVector<T*>::iterator sect_iter;
			for (sect_iter = m_Sectors.begin(); sect_iter != m_Sectors.end(); ++sect_iter)
			{
				delete [] *sect_iter;
			}
			m_Sectors.clear();
		}
	public:
		friend class iterator;
		class iterator
		{
			CStack<T> *m_pParent;
			size_t m_Index;
		public:
			iterator(CStack<T> *pParent, size_t id) : m_pParent(pParent), m_Index(id)
			{
			}

			iterator(CStack<T> *pParent) : m_pParent(pParent), m_Index(0)
			{
			}
			
			iterator() : m_pParent(NULL), m_Index(0)
			{
			}

			T &operator *()
			{
				return m_pParent->at(m_Index);
			}
			const T &operator *() const
			{
				return m_pParent->at(m_Index);
			}
			
			T * operator->()
			{
				return &(m_pParent->at(m_Index));
			}

			const T * operator->() const
			{
				return &(m_pParent->at(m_Index));
			}

			iterator & operator++()		// preincrement
			{
				++m_Index;
				return (*this);
			}

			iterator operator++(int)	// postincrement
			{
				iterator tmp = *this;
				++m_Index;
				return tmp;
			}

			iterator & operator--()		// predecrement
			{
				--m_Index;
				return (*this);
			}

			iterator operator--(int)	// postdecrememnt
			{
				iterator tmp = *this;
				--m_Index;
				return tmp;
			}

			bool operator==(const iterator & right) const
			{
				return (m_pParent == right.m_pParent && m_Index == right.m_Index);
			}

			bool operator!=(const iterator & right) const
			{
				return !(*this == right);
			}
		};
		CStack() : m_UsedSize(0)
		{
		}

		CStack(const CStack &other)
		{
			for (typename CVector<T*>::iterator sect_iter = other.m_Sectors.begin();
				sect_iter != other.m_Sectors.end(); ++sect_iter)
			{
				m_Sectors.push_back(new T[SECTOR_SIZE]);
				for (size_t i = 0; i < SECTOR_SIZE; ++i)
					m_Sectors.back()[i] = (*sect_iter)[i];
			}
			m_UsedSize = other.m_UsedSize;
		}

		CStack & operator =(const CStack &other)
		{
			clear();
			for (typename CVector<T*>::iterator sect_iter = other.m_Sectors.begin();
				sect_iter != other.m_Sectors.end(); ++sect_iter)
			{
				m_Sectors.push_back(new T[SECTOR_SIZE]);
				for (size_t i = 0; i < SECTOR_SIZE; ++i)
					m_Sectors.back()[i] = (*sect_iter)[i];
			}
			m_UsedSize = other.m_UsedSize;
			return *this;
		}

		~CStack()
		{
			clear();
		}

		T &at(size_t x)
		{
			return m_Sectors[x / SECTOR_SIZE][x % SECTOR_SIZE];
		}

		const T &at(size_t x) const
		{
			return m_Sectors[x / SECTOR_SIZE][x % SECTOR_SIZE];
		}

		bool push(const T &val)
		{
			if ((m_UsedSize / SECTOR_SIZE) >= m_Sectors.size())
			{
				// New sector
				m_Sectors.push_back(new T[SECTOR_SIZE]);
			}

			at(m_UsedSize) = val;

			++m_UsedSize;
			return true;
		}

		T *make_next()
		{
			if ((m_UsedSize / SECTOR_SIZE) >= m_Sectors.size())
			{
				// New sector
				m_Sectors.push_back(new T[SECTOR_SIZE]);
			}

			return &(at(m_UsedSize++));
		}

		void pop()
		{
			--m_UsedSize;
		}

		void popall()
		{
			m_UsedSize = 0;
		}

		T &front()
		{
			return at(m_UsedSize-1);
		}

		const T &front() const
		{
			return at(m_UsedSize-1);
		}

		T &second()
		{
			return at(m_UsedSize-2);
		}

		const T &second() const
		{
			return at(m_UsedSize-2);
		}

		iterator begin()
		{
			return iterator(this, 0);
		}
		iterator end()
		{
			return iterator(this, m_UsedSize);
		}

		size_t size()
		{
			return m_UsedSize;
		}
		bool empty()
		{
			return m_UsedSize == 0 ? true : false;
		}
	};
};	//namespace SourceHook

#endif
