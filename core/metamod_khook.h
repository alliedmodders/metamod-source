/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2026 AlliedModders LLC and authors.
 * All rights reserved.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose, 
 * including commercial applications, and to alter it and redistribute it 
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not 
 * claim that you wrote the original software. If you use this software in a 
 * product, an acknowledgment in the product documentation would be 
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id$
 */
#pragma once

#include <khook.hpp>
#include <vector>

#ifndef KHOOK_STANDALONE
static_assert(false, "KHOOK_STANDALONE wasn't defined!");
#endif

class KHookImpl : public KHook::IKHook {
public:
	~KHookImpl() {
		for (auto id : m_hooks) {
			KHook::RemoveHook(id, true);
		}
	}

	virtual KHook::HookID_t SetupHook(
		void* function,
		void* context,
		void* removed_function,
		void* pre,
		void* post,
		void* make_return,
		void* make_call_original,
		unsigned int stack_size, 
		bool async = false
	) override {
		return KHook::SetupHook(
			function,
			context,
			removed_function,
			pre,
			post,
			make_return,
			make_call_original,
			stack_size,
			async
		);
	}
	virtual KHook::HookID_t SetupVirtualHook(
		void** vtable,
		int index,
		void* context,
		void* removed_function,
		void* pre,
		void* post,
		void* make_return,
		void* make_call_original,
		unsigned int stack_size,
		bool async = false
	) override {
		auto id = KHook::SetupVirtualHook(
			vtable,
			index,
			context,
			removed_function,
			pre,
			post,
			make_return,
			make_call_original,
			stack_size,
			async
		);
		if (id != KHook::INVALID_HOOK) {
			m_hooks.push_back(id);
		}
		return id;
	}
	virtual void RemoveHook(KHook::HookID_t id, bool async = false) override {
		return KHook::RemoveHook(id, async);
	}
	virtual void* GetContextPtr() override {
		return KHook::GetContextPtr();
	}
	virtual void* GetOriginalFunction() override {
		return KHook::GetOriginalFunction();
	}
	virtual void* GetOriginalValuePtr() override {
		return KHook::GetOriginalValuePtr();
	}
	virtual void* GetOverrideValuePtr() override {
		return KHook::GetOverrideValuePtr();
	}
	virtual void* GetCurrentValuePtr(bool pop = false) override {
		return KHook::GetCurrentValuePtr(pop);
	}
	virtual void DestroyReturnValue() override {
		return KHook::DestroyReturnValue();
	}
	virtual void* DoRecall(KHook::Action action, void* ptr_to_return, std::size_t return_size, void* init_op, void* delete_op) override {
		return KHook::DoRecall(action, ptr_to_return, return_size, init_op, delete_op);
	}
	virtual void SaveReturnValue(KHook::Action action, void* ptr_to_return, std::size_t return_size, void* init_op, void* delete_op, bool original) {
		return KHook::SaveReturnValue(action, ptr_to_return, return_size, init_op, delete_op, original);
	}
	virtual void* FindOriginal(void* function) override {
		return KHook::FindOriginal(function);
	}
	virtual void* FindOriginalVirtual(void** vtable, int index) override {
		return KHook::FindOriginalVirtual(vtable, index);
	}
	virtual void* LookupSignature(void* start, std::size_t size, const char* signature) override {
		return KHook::LookupSignature(start, size, signature);
	}
protected:
	std::vector<KHook::HookID_t> m_hooks;
};