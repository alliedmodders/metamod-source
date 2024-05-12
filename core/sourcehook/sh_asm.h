#pragma once

#include <cstdint>
#include "sh_pagealloc.h"

namespace SourceHook
{
	namespace Asm
	{
		class GenBuffer
		{
			static CPageAlloc ms_Allocator;

			unsigned char *m_pData;
			std::uint32_t m_Size;
			std::uint32_t m_AllocatedSize;

		public:
			GenBuffer() : m_pData(NULL), m_Size(0), m_AllocatedSize(0)
			{
			}
			~GenBuffer()
			{
				clear();
			}
			std::uint32_t GetSize()
			{
				return m_Size;
			}
			unsigned char *GetData()
			{
				return m_pData;
			}

			template <class PT> void push(PT what)
			{
				push((const unsigned char *)&what, sizeof(PT));
			}

			void push(const unsigned char *data, std::uint32_t size)
			{
				std::uint32_t newSize = m_Size + size;
				if (newSize > m_AllocatedSize)
				{
					m_AllocatedSize = newSize > m_AllocatedSize*2 ? newSize : m_AllocatedSize*2;
					if (m_AllocatedSize < 64)
						m_AllocatedSize = 64;

					unsigned char *newBuf;
					newBuf = reinterpret_cast<unsigned char*>(ms_Allocator.Alloc(m_AllocatedSize));
					ms_Allocator.SetRW(newBuf);
					if (!newBuf)
					{
						SH_ASSERT(0, ("bad_alloc: couldn't allocate 0x%08X bytes of memory\n", m_AllocatedSize));
						return;
					}
					memset((void*)newBuf, 0xCC, m_AllocatedSize);			// :TODO: remove this !
					memcpy((void*)newBuf, (const void*)m_pData, m_Size);
					if (m_pData)
					{
						ms_Allocator.SetRE(reinterpret_cast<void*>(m_pData));
						ms_Allocator.SetRW(newBuf);
						ms_Allocator.Free(reinterpret_cast<void*>(m_pData));
					}
					m_pData = newBuf;
				}
				memcpy((void*)(m_pData + m_Size), (const void*)data, size);
				m_Size = newSize;
			}

			template <class PT> void rewrite(std::uint32_t offset, PT what)
			{
				rewrite(offset, (const unsigned char *)&what, sizeof(PT));
			}

			void rewrite(std::uint32_t offset, const unsigned char *data, std::uint32_t size)
			{
				SH_ASSERT(offset + size <= m_AllocatedSize, ("rewrite too far"));

				memcpy((void*)(m_pData + offset), (const void*)data, size);
			}

			void clear()
			{
				if (m_pData)
					ms_Allocator.Free(reinterpret_cast<void*>(m_pData));
				m_pData = NULL;
				m_Size = 0;
				m_AllocatedSize = 0;
			}

			void SetRE()
			{
				ms_Allocator.SetRE(reinterpret_cast<void*>(m_pData));
			}

			operator void *()
			{
				return reinterpret_cast<void*>(GetData());
			}

			void write_ubyte(std::uint8_t x)		{ push(x); }
			void write_byte(std::int8_t x)			{ push(x); }
			
			void write_ushort(unsigned short x)		{ push(x); }
			void write_short(signed short x)		{ push(x); }

			void write_uint32(std::uint32_t x)		{ push(x); }
			void write_int32(std::int32_t x)		{ push(x); }

			void write_uint64(std::uint64_t x)		{ push(x); }
			void write_int64(std::int64_t x)		{ push(x); }

			std::uint32_t get_outputpos()
			{
				return m_Size;
			}

			void start_count(std::uint32_t &offs)
			{
				offs = get_outputpos();
			}
			void end_count(std::uint32_t &offs)
			{
				offs = get_outputpos() - offs;
			}
		};
	}
}