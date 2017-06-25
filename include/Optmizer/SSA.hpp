#pragma once

#include "AST/ASTNode.hpp"
#include "Basic/Scope.hpp"
#include <unordered_map>
#include <memory>
#include <vector>
#include <string>

namespace opt {
namespace ssa {

template <class... Fns>
struct overloader : Fns...
{
	using Fns::operator()...;
	overloader(Fns... fns) : Fns(fns)...{}
};

// Temporary 
template <class Visitor, class Ty>
void visit(Visitor V, Ty T) {
	V(T);
}

template <class T, class U>
T get_if(U) {

}

/*
 * Keeps track of the various versions of ssa variables.
 */
class SSAVarDeclVersioning
{
public:
	VarDeclNode *makeNew(Scope *S, VarDeclNode *Var) {
		auto *Decl = S->makeVarDecl(Var->getName(), Var->getType());
		return mVarDecls.emplace_back(Decl);
	}

	VarDeclNode *mostRecent() {
		return mVarDecls.back()->get();
	}

private:
	std::vector<std::unique_ptr<VarDeclNode>> mVarDecls;
	size_t Cnt;
};

class SSAScopedVarDeclList
{
public:
	SSAScopedVarDeclList(Scope *S, SSAScopedVarDeclList *OuterList)
		: mScope(S)
		, mOuterList(OuterList) {}

	VarDeclRefNode *makeNewDecl(VarDeclNode *Old) {
		if (Old->getScope() != mScope) {
			if (mOuterList == nullptr) throw int(1);

			return mOuterList->makeNewDecl(Old);
		}

		return mScopedDecls[Old].makeNew();
	}

	VarDeclRefNode *replaceRef(VarDeclRefNode *Old) {
		auto *NewDecl = makeNewDecl(Old->getDecl());
		
	}
private:
	SSAScopedVarDeclList *mOuterList;
	std::vector<std::unique_ptr<SSAScopedVarDeclList>> mInnerLists;

	Scope *mScope;
	std::unordered_map<VarDeclNode *, SSAVarDeclVersioning> mScopedDecls;

	std::vector<std::unique_ptr<VarDeclRefNode>> mVarRefs;
}

class SSAPass
{
public:
	SSAPass(Scope *FileScope)
		: mFileScope(FileScope) {}

	void run(Scope *FileScope) {
		SSAScopedVarDeclList GlobalList{FileScope, nullptr};
		scopePass(FileScope, &GlobalList);
	}

	void scopePass(Scope *S, SSAScopedVarDeclList *VarDeclList) {
		CompoundStmtNode CSNReplace;

		
		auto *CompoundStmt = S->getCompoundStmt();

		for (auto &PNode : CompoundStmt.statements()) {
			// Ignore variable declarations
			if (std::get_if<ast::VarDeclNode *>(&PNode))
				continue;

			if (auto **PAssign = std::get_if<ast::AssignNode *>(*PNode)) {
				auto *Assign = *PAssign;

				auto [Lhs, Rhs] = Assign->getDecomposedOperands();

				auto *SSAVar = VarDecls[Lhs->getDeclaration()->getName()].makeNew(ScopeReplace.get());

				using std::visit;

				visit(overloader(
					[Assign](VarDeclRefNode *Node) {
						Assign->replaceRhs(VarDecls[Node->getDecl()->getName()].mostRecent());
					},
					[Assign](auto *Node) {
						if constexpr(is_binary_op_v<decltype(*Node)>) {
							auto [Lhs, Rhs] = Node->getDecomposedOperands();

							if (auto **PLN = std::get_if<VarDeclRefNode *>(Lhs)) {
								auto *LN = *PLN;
								Node->replaceLhs(VarDecls[LN->getDecl()->getName()].mostRecent());
							}

							if (auto **PRN = std::get_if<VarDeclRefNode *>(Rhs)) {
								auto *RN = *PRN;
								Node->replaceRhs(VarDecls[RN->getDecl()->getName()].mostRecent());
							}
						} else if (is_unary_op_v<decltype(*Node)>) {
							auto *Op = Node->getOperand();
							if (auto **PN = std::get_if<VarDeclRefNode>(Op)) {
								auto *N = *PN;
								Node->replaceOperand(VarDecls[RN->getDecl()->getName()].mostRecent());
							}
						}
					}
				));
			}

			CSNReplace.addStmt(PNode);
		}



	}
private:
	Scope *mFileScope;
}

} // namesapce ssa
} // namespace opt