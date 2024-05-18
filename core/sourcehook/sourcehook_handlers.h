/* ======== SourceHook ========
* Copyright (C) 2004-2023 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): mixern6 (SVRL_)
* ============================
*/

/**
*	@file sourcehook_handlers.h
*	@brief Contains the public SourceHook API
*/

#ifndef __SOURCEHOOK_HANDLERS_H__
#define __SOURCEHOOK_HANDLERS_H__

#include "sourcehook.h"
#include <type_traits>

namespace SourceHook
{
class ISourceHook;
typedef int Plugin;
}

extern SourceHook::ISourceHook *g_SHPtr;
extern SourceHook::Plugin g_PLID;

namespace SourceHook
{

// Get type info for type T
template<typename T>
inline void TypeInfo(PassInfo& passInfo)
{
    passInfo.size = sizeof(T);
    passInfo.type = GetPassInfo<T>::type;
    passInfo.flags = GetPassInfo<T>::flags;
}

// Get type info for type T
template<>
inline void TypeInfo<void>(PassInfo& passInfo)
{
    passInfo.size = 1;
    passInfo.type = 0;
    passInfo.flags = 0;
}

template<typename... ArgsType>
class PassInfoInitializer
{
public:
    constexpr PassInfoInitializer()
    {
        InitializePassInfo<sizeof...(ArgsType), 0, ArgsType...>();
    }

    const PassInfo *GetParamsPassInfo() const
    {
        return params_;
    }

    const PassInfo::V2Info *GetParamsPassInfoV2() const
    {
        return paramsV2_;
    }

    const size_t GetParamsPassInfoSize() const
    {
        return sizeof...(ArgsType);
    }

private:
    template<size_t size, size_t index, typename T, typename... NextArgsType>
    inline typename std::enable_if<sizeof...(NextArgsType) != 0, void>::type InitializePassInfo()
    {
        TypeInfo<T>(params_[index]);
        InitializePassInfo<size, index + 1, NextArgsType...>();
    }

    template<size_t size, size_t index, typename T>
    inline void InitializePassInfo()
    {
        TypeInfo<T>(params_[index]);
    }

    PassInfo params_[sizeof...(ArgsType)];
    PassInfo::V2Info paramsV2_[sizeof...(ArgsType)];
};

// For zero arguments
template<>
class PassInfoInitializer<>
{
public:
    constexpr PassInfoInitializer()
    {
    }

    const PassInfo *GetParamsPassInfo() const
    {
        return nullptr;
    }

    const PassInfo::V2Info *GetParamsPassInfoV2() const
    {
        return nullptr;
    }

    const size_t GetParamsPassInfoSize() const
    {
        return 0;
    }
};

template <typename T>
struct ReturnTypeInfo
{
    static const size_t size()
    {
        return sizeof(T);
    }

    static const int type()
    {
        return GetPassInfo<T>::type;
    }

    static const unsigned int flags()
    {
        return GetPassInfo<T>::flags;
    }
};

template <>
struct ReturnTypeInfo<void>
{
    static const size_t size()
    {
        return 0;
    }

    static const int type()
    {
        return 0;
    }

    static const unsigned int flags()
    {
        return 0;
    }
};

template<typename T>
class CHookManagerMemberFuncHandler : public IHookManagerMemberFunc
{
public:
    typedef int (T::*HookManagerMemberFunc)(bool store, IHookManagerInfo *hi);

    explicit CHookManagerMemberFuncHandler(T* funcHandler, HookManagerMemberFunc func)
     : funcHandler_(funcHandler)
     , func_(func)
    {
    }

    virtual ~CHookManagerMemberFuncHandler()
    {
    }

private:
    virtual int Call(bool store, IHookManagerInfo *hi) const override
    {
        return (funcHandler_->*func_)(store, hi);
    }

private:
    T* funcHandler_;
    HookManagerMemberFunc func_;
};

template<typename ReturnType, class ... Params>
class CProtoInfo : public IProtoInfo
{
public:
	constexpr CProtoInfo()
         : retPassInfo(ReturnTypeInfo<ReturnType>::size(),
		       ReturnTypeInfo<ReturnType>::type(),
		       ReturnTypeInfo<ReturnType>::flags())
	{
	}

	virtual ~CProtoInfo() override
	{
	}

	virtual size_t GetNumOfParams() const override
	{
		return paramsPassInfo.GetParamsPassInfoSize();
	}

	virtual const PassInfo &GetRetPassInfo() const override
	{
		return retPassInfo;
	}

	virtual const PassInfo *GetParamsPassInfo() const override
	{
		return paramsPassInfo.GetParamsPassInfo();
	}

	virtual int GetConvention() const override
	{
		return 0;
	}

	// version of the ProtoInfo structure.
	virtual IProtoInfo::ProtoInfoVersion GetVersion() const override
	{
		return ProtoInfoVersion::ProtoInfoVersion2;
	}

	virtual const PassInfo::V2Info &GetRetPassInfo2() const override
	{
		return retPassInfo2;
	}

	virtual const PassInfo::V2Info *GetParamsPassInfo2() const override
	{
		return paramsPassInfo.GetParamsPassInfoV2();
	}

private:
	int numOfParams;		//!< number of parameters
	PassInfo retPassInfo;		//!< PassInfo for the return value. size=0 -> no retval
	PassInfo::V2Info retPassInfo2;  //!< Version2 only
	PassInfoInitializer<Params...> paramsPassInfo;	//!< PassInfos for the parameters
};

template<typename ReturnType, class ... Params>
class ManualHookHandler : public CHookManagerMemberFuncHandler<ManualHookHandler<ReturnType, Params...>>
{
public:
    typedef ManualHookHandler<ReturnType, Params...> ThisType;
    typedef ReturnType(EmptyClass::*ECMFP)(Params...);
    typedef ExecutableClassN<EmptyClass, ECMFP, ReturnType, Params...> CallEC;

    template<typename U>
    struct HookedMemberFuncType
    {
        typedef ReturnType (U::*HookFunc)(Params...);
    };

    struct HookedStaticFuncType
    {
        typedef ReturnType (*HookFunc)(Params...);
    };

    template<typename T, typename U>
    struct HookedMemberFuncIFaceType
    {
        typedef ReturnType (U::*HookFunc)(T*, Params...);
    };

    template<typename T>
    struct HookedStaticFuncIFaceType
    {
        typedef ReturnType (*HookFunc)(T*, Params...);
    };

    constexpr ManualHookHandler()
        : CHookManagerMemberFuncHandler<ThisType>(this, &ThisType::HookManPubFunc)
        , msMFI_{false, 0, 0, 0}
        , msHI_(nullptr)
    {
    }

    ManualHookHandler(const ManualHookHandler&) = delete;

    ManualHookHandler(ManualHookHandler&&) = delete;

    ManualHookHandler& operator=(const ManualHookHandler&) = delete;

    ManualHookHandler& operator=(ManualHookHandler&&) = delete;

    virtual ~ManualHookHandler()
    {
        HookManagerPubFuncHandler pubFunc(this);
        g_SHPtr->RemoveHookManager(g_PLID, pubFunc);
    }

    void Reconfigure(int vtblindex, int vtbloffs = 0, int thisptroffs = 0)
    {
        HookManagerPubFuncHandler pubFunc(this);
        g_SHPtr->RemoveHookManager(g_PLID, pubFunc);
        msMFI_.thisptroffs = thisptroffs;
        msMFI_.vtblindex = vtblindex;
        msMFI_.vtbloffs = vtbloffs;
    }

    // Hook with a statuc function callback
    // ReturnType Callback(Params...);
    [[nodiscard]] int Add(void *iface,
                          typename HookedStaticFuncType::HookFunc callbackFuncPtr,
                          bool post = true,
                          ISourceHook::AddHookMode mode = ISourceHook::AddHookMode::Hook_Normal)
    {
        HookManagerPubFuncHandler pubFunc(this);
        SHDelegateHandler shDelegate = SHDelegateHandler::Make<ThisType::CStaticDelegateImpl>(callbackFuncPtr);
        return g_SHPtr->AddHook(g_PLID, mode, iface, 0, pubFunc, shDelegate, post);
    }

    // Hook with a statuc function callback with iface pointer first argument
    // ReturnType Callback(T *iface, Params...);
    template<typename T>
    [[nodiscard]] int Add(T *iface,
                          typename HookedStaticFuncIFaceType<T>::HookFunc callbackFuncPtr,
                          bool post = true,
                          ISourceHook::AddHookMode mode = ISourceHook::AddHookMode::Hook_Normal)
    {
        HookManagerPubFuncHandler pubFunc(this);
        SHDelegateHandler shDelegate = SHDelegateHandler::Make<ThisType::CStaticIFaceDelegateImpl<T>>(callbackFuncPtr);
        return g_SHPtr->AddHook(g_PLID, mode, iface, 0, pubFunc, shDelegate, post);
    }

    // Hook with a member function callback
    // ReturnType U::Callback(Params...);
    template<typename U, typename std::enable_if<std::is_class<U>::value, void>::type* = nullptr>
    [[nodiscard]] int Add(void *iface,
                          U* callbackInstPtr,
                          typename HookedMemberFuncType<U>::HookFunc callbackFuncPtr,
                          bool post = true,
                          ISourceHook::AddHookMode mode = ISourceHook::AddHookMode::Hook_Normal)
    {
        HookManagerPubFuncHandler pubFunc(this);
        SHDelegateHandler shDelegate = SHDelegateHandler::Make<ThisType::CMemberDelegateImpl<U>>(callbackInstPtr, callbackFuncPtr);
        return g_SHPtr->AddHook(g_PLID, mode, iface, 0, pubFunc, shDelegate, post);
    }

    // Hook with a member function callback with iface pointer first argument
    // ReturnType U::Callback(T *iface, Params...);
    template<typename T, typename U, typename std::enable_if<std::is_class<U>::value, void>::type* = nullptr>
    [[nodiscard]] int Add(T *iface,
                          U *callbackInstPtr,
                          typename HookedMemberFuncIFaceType<T, U>::HookFunc callbackFuncPtr,
                          bool post = true,
                          ISourceHook::AddHookMode mode = ISourceHook::AddHookMode::Hook_Normal)
    {
        HookManagerPubFuncHandler pubFunc(this);
        SHDelegateHandler shDelegate = SHDelegateHandler::Make<ThisType::CMemberIFaceDelegateImpl<T, U>>(callbackInstPtr, callbackFuncPtr);
        return g_SHPtr->AddHook(g_PLID, mode, iface, 0, pubFunc, shDelegate, post);
    }

    inline bool Remove(int hookid)
    {
        return g_SHPtr->RemoveHookByID(hookid);
    }

    inline bool Pause(int hookid)
    {
        return g_SHPtr->PauseHookByID(hookid);
    }

    inline bool Unpause(int hookid)
    {
        return g_SHPtr->UnpauseHookByID(hookid);
    }

    // See RETURN_META_MNEWPARAMS
    // For void return type only
    template<typename U = ReturnType, typename std::enable_if<std::is_same<U, void>::value, void>::type* = nullptr>
    void Recall(META_RES result, Params... newparams)
    {
        g_SHPtr->SetRes(result);
        g_SHPtr->DoRecall();
        SourceHook::EmptyClass *thisptr = reinterpret_cast<EmptyClass*>(g_SHPtr->GetIfacePtr());
        (thisptr->*(GetRecallMFP(thisptr)))(newparams...);
        g_SHPtr->SetRes(MRES_SUPERCEDE);
    }

    // See RETURN_META_VALUE_MNEWPARAMS
    // For any return type but void
    template<typename U = ReturnType, typename std::enable_if<!std::is_same<U, void>::value, void>::type* = nullptr>
    [[nodiscard]] U Recall(META_RES result, U value, Params... newparams)
    {
        g_SHPtr->SetRes(result);
        g_SHPtr->DoRecall();
        if ((result) >= MRES_OVERRIDE)
        {
            SetOverrideResult(g_SHPtr, value);
        }
        EmptyClass *thisptr = reinterpret_cast<EmptyClass*>(g_SHPtr->GetIfacePtr());
        g_SHPtr->SetRes(MRES_SUPERCEDE);
        return (thisptr->*(GetRecallMFP(thisptr)))(newparams...);
    }

    // See RETURN_META.
    // NOTE: for RETURN_META_VALUE return the value after this call
    inline void SetResult(META_RES res)
    {
        g_SHPtr->SetRes(res);
    }

    [[nodiscard]] inline META_RES GetPreviousResult() const
    {
        return g_SHPtr->GetPrevRes();
    }

    [[nodiscard]] inline META_RES GetStatus() const
    {
        return g_SHPtr->GetStatus();
    }

private:
    struct IMyDelegate : ISHDelegate
    {
        virtual ReturnType Call(Params... params) = 0;

        // Unneeded
        // old SH_REMOVE_HOOK syntax is not supported
        virtual bool IsEqual(ISHDelegate */*pOtherDeleg*/) override
        {
            SH_ASSERT(false, ("Forbidden use of ISHDelegate::IsEqual"));
            return false;
        }
    };

    struct CStaticDelegateImpl : public IMyDelegate
    {
        typename HookedStaticFuncType::HookFunc funcPtr_;
        CStaticDelegateImpl(typename HookedStaticFuncType::HookFunc callbackFuncPtr)
            : funcPtr_(callbackFuncPtr)
        {
        }

        virtual ReturnType Call(Params... params) override
        {
            return funcPtr_(params...);
        }

        virtual void DeleteThis() override
        {
            delete this;
        }
    };

    template<typename T>
    struct CStaticIFaceDelegateImpl : public IMyDelegate
    {
        typename HookedStaticFuncIFaceType<T>::HookFunc funcPtr_;
        CStaticIFaceDelegateImpl(typename HookedStaticFuncIFaceType<T>::HookFunc callbackFuncPtr)
            : funcPtr_(callbackFuncPtr)
        {
        }

        virtual ReturnType Call(Params... params) override
        {
            return funcPtr_(reinterpret_cast<T*>(g_SHPtr->GetIfacePtr()), params...);
        }

        virtual void DeleteThis() override
        {
            delete this;
        }
    };

    template<typename U>
    struct CMemberDelegateImpl : public IMyDelegate
    {
        U* instPtr_;
        typename HookedMemberFuncType<U>::HookFunc funcPtr_;
        CMemberDelegateImpl(U* callbackInstPtr, typename HookedMemberFuncType<U>::HookFunc callbackFuncPtr)
            : instPtr_(callbackInstPtr)
            , funcPtr_(callbackFuncPtr)
        {
        }

        virtual ReturnType Call(Params... params) override
        {
            return (instPtr_->*funcPtr_)(params...);
        }

        virtual void DeleteThis() override
        {
            delete this;
        }
    };

    template<typename T, typename U>
    struct CMemberIFaceDelegateImpl : public IMyDelegate
    {
        U* instPtr_;
        typename HookedMemberFuncIFaceType<T, U>::HookFunc funcPtr_;
        CMemberIFaceDelegateImpl(U* callbackInstPtr, typename HookedMemberFuncIFaceType<T, U>::HookFunc callbackFuncPtr)
            : instPtr_(callbackInstPtr)
            , funcPtr_(callbackFuncPtr)
        {
        }

        virtual ReturnType Call(Params... params) override
        {
            return (instPtr_->*funcPtr_)(reinterpret_cast<T*>(g_SHPtr->GetIfacePtr()), params...);
        }

        virtual void DeleteThis() override
        {
            delete this;
        }
    };

    // Implementation for any return type but void
    template<typename U = ReturnType, typename std::enable_if<!std::is_same<U, void>::value, void>::type* = nullptr>
    U Func(Params... params)
    {
        /* 1) Set up calls */
        void *ourvfnptr = reinterpret_cast<void*>(*reinterpret_cast<void***>(reinterpret_cast<char*>(this) + msMFI_.vtbloffs) + msMFI_.vtblindex);
        void *vfnptr_origentry;

        META_RES status = MRES_IGNORED;
        META_RES prev_res;
        META_RES cur_res;

        typedef typename ReferenceCarrier<ReturnType>::type my_rettype;
        my_rettype orig_ret;
        my_rettype override_ret;
        my_rettype plugin_ret;
        IMyDelegate *iter = nullptr;

        IHookContext *pContext = g_SHPtr->SetupHookLoop(msHI_,
                                                        ourvfnptr,
                                                        reinterpret_cast<void*>(this),
                                                        &vfnptr_origentry,
                                                        &status,
                                                        &prev_res,
                                                        &cur_res,
                                                        &orig_ret,
                                                        &override_ret);

        prev_res = MRES_IGNORED;

        while ((iter = static_cast<IMyDelegate*>(pContext->GetNext())))
        {
            cur_res = MRES_IGNORED;
            plugin_ret = iter->Call(params...);
            prev_res = cur_res;

            if (cur_res > status)
                status = cur_res;

            if (cur_res >= MRES_OVERRIDE)
                *reinterpret_cast<my_rettype*>(pContext->GetOverrideRetPtr()) = plugin_ret;
        }

        if (status != MRES_SUPERCEDE && pContext->ShouldCallOrig())
        {
            ReturnType (EmptyClass::*mfp)(Params...);
            SH_SETUP_MFP(mfp);

            orig_ret = (reinterpret_cast<EmptyClass*>(this)->*mfp)(params...);
        }
        else
            orig_ret = override_ret; /* :TODO: ??? : use pContext->GetOverrideRetPtr() or not? */

        prev_res = MRES_IGNORED;
        while ((iter = static_cast<IMyDelegate*>(pContext->GetNext())))
        {
            cur_res = MRES_IGNORED;
            plugin_ret = iter->Call(params...);
            prev_res = cur_res;

            if (cur_res > status)
                status = cur_res;

            if (cur_res >= MRES_OVERRIDE)
                *reinterpret_cast<my_rettype*>(pContext->GetOverrideRetPtr()) = plugin_ret;
        }

        const void* repPtr = (status >= MRES_OVERRIDE) ? pContext->GetOverrideRetPtr() :
                                                         pContext->GetOrigRetPtr();

        g_SHPtr->EndContext(pContext);

        return *reinterpret_cast<const my_rettype*>(repPtr);
    }

    // Implementation for void return type only
    template<typename U = ReturnType, typename std::enable_if<std::is_same<U, void>::value, void>::type* = nullptr>
    void Func(Params... params)
    {
        /* 1) Set up calls */
        void *ourvfnptr = reinterpret_cast<void*>(*reinterpret_cast<void***>(reinterpret_cast<char*>(this) + msMFI_.vtbloffs) + msMFI_.vtblindex);
        void *vfnptr_origentry;

        META_RES status = MRES_IGNORED;
        META_RES prev_res;
        META_RES cur_res;

        IMyDelegate *iter = nullptr;

        IHookContext *pContext = g_SHPtr->SetupHookLoop(msHI_,
                                                        ourvfnptr,
                                                        reinterpret_cast<void*>(this),
                                                        &vfnptr_origentry,
                                                        &status,
                                                        &prev_res,
                                                        &cur_res,
                                                        nullptr,
                                                        nullptr);

        prev_res = MRES_IGNORED;
        while ((iter = static_cast<IMyDelegate*>(pContext->GetNext())))
        {
            cur_res = MRES_IGNORED;
            iter->Call(params...);
            prev_res = cur_res;

            if (cur_res > status)
                status = cur_res;
        }

        if (status != MRES_SUPERCEDE && pContext->ShouldCallOrig())
        {
            void (EmptyClass::*mfp)(Params...);
            SH_SETUP_MFP(mfp);

            (reinterpret_cast<EmptyClass*>(this)->*mfp)(params...);
        }

        prev_res = MRES_IGNORED;
        while ( (iter = static_cast<IMyDelegate*>(pContext->GetNext())))
        {
            cur_res = MRES_IGNORED;

            iter->Call(params...);
            prev_res = cur_res;

            if (cur_res > status)
                status = cur_res;
        }

        g_SHPtr->EndContext(pContext);
    }

    int HookManPubFunc(bool store, IHookManagerInfo *hi)
    {
        /* Verify interface version */
        if (g_SHPtr->GetIfaceVersion() != SH_IFACE_VERSION)
            return 1;

        if (g_SHPtr->GetImplVersion() < SH_IMPL_VERSION)
            return 1;

        if (store)
            msHI_ = hi;

        if (hi)
        {
            MemFuncInfo mfi = {true, -1, 0, 0};
            GetFuncInfo<ThisType, ThisType, ReturnType, Params...>(this, &ThisType::Func, mfi);

            hi->SetInfo(SH_HOOKMAN_VERSION,
                        msMFI_.vtbloffs,
                        msMFI_.vtblindex,
                        &msProto_,
                        reinterpret_cast<void**>(reinterpret_cast<char*>(this) + mfi.vtbloffs)[mfi.vtblindex]);
        }

        return 0;
    }

    typename ManualHookHandler::ECMFP GetRecallMFP(EmptyClass *thisptr)
    {
        union
        {
            typename ManualHookHandler::ECMFP mfp;
            struct
            {
                void *addr;
                intptr_t adjustor;
            } s;
        } u;

        u.s.addr = (*reinterpret_cast<void***>(reinterpret_cast<char*>(thisptr) + msMFI_.vtbloffs))[msMFI_.vtblindex];
        u.s.adjustor = 0;

        return u.mfp;
    }

    typename ManualHookHandler::CallEC Call(void *ptr)
    {
        typename ManualHookHandler::ECMFP mfp;
        void *vfnptr = reinterpret_cast<void*>(*reinterpret_cast<void***>((reinterpret_cast<char*>(ptr) + msMFI_.thisptroffs + msMFI_.vtbloffs) ) + msMFI_.vtblindex);

        /* patch mfp */
        *reinterpret_cast<void**>(&mfp) = *reinterpret_cast<void**>(vfnptr);

        if (sizeof(mfp) == 2*sizeof(void*)) /* gcc */
            *(reinterpret_cast<void**>(&mfp) + 1) = 0;

        return ManualHookHandler::CallEC(reinterpret_cast<EmptyClass*>(ptr), mfp, vfnptr, g_SHPtr);
    }

    // Implementation for any return type but void
    template<typename U = ReturnType, typename std::enable_if<!std::is_same<U, void>::value, void>::type* = nullptr>
    void SetOverrideResult(ISourceHook *shptr, U value)
    {
        OverrideFunctor<U> overrideFunc = SourceHook::SetOverrideResult<U>();
        overrideFunc(shptr, value);
    }

private:
    MemFuncInfo msMFI_; // ms_MFI
    IHookManagerInfo *msHI_; // ms_HI

    CProtoInfo<ReturnType, Params...> msProto_; // ms_Proto
};

} // SourceHook

#endif  // __SOURCEHOOK_HANDLERS_H__
