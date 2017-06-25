#pragma once

#include "ASTNode.hpp"
#include "Basic/Scope.hpp"
#include <string_view>
#include <vector>
#include <memory>
#include <utility>


namespace ast {



class Context
{
public:
	Context()
		: mGlobalScope(nullptr) {}

	Scope *globalScope() {
		return &mGlobalScope;
	}

	Scope *openScope() {
		auto *NewScope = mCurrScope->openScope();
		return (mCurrScope = NewScope);
	}






private:
	Scope mGlobalScope;
	Scope *mCurrScope = &mGlobalScope;

};

} // namespace ast