#pragma once

#include <memory>
#include <vector>

#include "AST/ASTNode.hpp"

using namespace ast;

using namespace ast::type;

class Scope
{
public:
    Scope(Scope *Outer)
        : mOuterScope(Outer)
        , mCompoundStmt(this) {}

    CompoundStmtNode *getCompoundStmt() {
        return &mCompoundStmt;
    }
/*
    bool operator==(Scope Rhs) const {
        return mCSN == Rhs.mCSN;
    }

    bool operator!=(Scope Rhs) const {
        return !(*this == Rhs);
    }

    bool isNestedOf(Scope S) const {

    }
*/
    Scope *openScope() {
        return mInnerScopes.emplace_back(std::make_unique<Scope>(this)).get();
    }

    Scope *closeScope() {
        return mOuterScope;
    }

    type::Compound *makeCompoundType(std::string_view Name, std::vector<Type> Types) {
        return mCompoundTypes.emplace_back(std::make_unique<type::Compound>(Name, std::move(Types)))
            .get();
    }

    VarDeclNode *makeVarDecl(std::string_view Name, Type Type) {
        auto *Node = mVarDecls.emplace_back(std::make_unique<VarDeclNode>(this, Name, Type))
            .get();
        mCompoundStmt.addStmt(Node);
        return Node;
    }

    VarDeclRefNode *makeRef(VarDeclNode *Var) {
    auto &Variant = mNodes.emplace_back(std::make_unique<VarDeclRefNode>(this, Var));
        auto *Node = std::get_if<std::unique_ptr<VarDeclRefNode>>(&Variant)->get();
        return Node;
    }

    template <class TConstant>
    ConstantNode *makeConstant(std::string_view Name, TConstant Constant) {
        auto *Node = mConstants.emplace_back(std::make_unique<ConstantNode>(this, Name, Constant))
            .get();
        mCompoundStmt.addStmt(Node);
        return Node;
    }
/*
    std::pair<CompoundStmtNode *, Scope> makeCompoundStmt(Scope *S) {
        auto *Node = mCompoundStmts.emplace_back(std::make_unique<CompoundStmtNode>(S))
            .get();
        S->addStmt(Node);
        return {Node, {Node}};
    }
    */

    FnNode *makeFunctionDecl(std::string_view Name, Type RetType, std::vector<Type> ParmTypes, CompoundStmtNode *FnBody) {
        type::Function FnType {RetType, std::move(ParmTypes)};
        auto *Node = mFunctions.emplace_back(std::make_unique<FnNode>(this, Name, FnType, FnBody))
            .get();
        mCompoundStmt.addStmt(Node);
        return Node;
    }

    FnNode *getFunction(std::string_view Name) {
        for (const auto &Fn : mFunctions) {
            if (Name == Fn->getName()) {
                return Fn.get();
            }
        }
        return nullptr;
    }

    CallNode *buildFunctionCall(std::string_view FnName, std::vector<VarOrConstant> CallArgs) {
        auto *Fn = getFunction(FnName);
        if (!Fn) {
            return nullptr;
        }
        auto *Node = mFnCalls.emplace_back(std::make_unique<CallNode>(this, Fn, std::move(CallArgs)))
            .get();
        mCompoundStmt.addStmt(Node);
        return Node;
    }

    template <class TConstant>
    ConstantNode *constant(TConstant Constant) {
        auto *Node = mConstants.emplace_back(std::make_unique<ConstantNode>(this, "", Constant))
            .get();
        mCompoundStmt.addStmt(Node);
        return Node;
    }

    AssignNode *assign(VarDeclRefNode *Lhs, AssignNode::AssignExpr Rhs) {
        auto &Var = mNodes.emplace_back(std::make_unique<AssignNode>(this, Lhs, Rhs));
        auto *Node = std::get_if<std::unique_ptr<AssignNode>>(&Var)->get();
        mCompoundStmt.addStmt(Node);
        return Node;
    }

    RelOpNode *compare(RelOpId Id, RelOpNode::Op1ExprType Lhs, RelOpNode::Op2ExprType Rhs) {
        auto *Node = mComparisons.emplace_back(std::make_unique<RelOpNode>(this, Id, Lhs, Rhs))
            .get();
        return Node;
    }

    Scope *getOuterScope() {
        return mOuterScope;
    }

    //void ssaReplace(ast::CompoundStmtNode NewCSN, std::vector<std::unique_ptr<VarDeclNode)

private:
    Scope *mOuterScope;
    ast::CompoundStmtNode mCompoundStmt;

    std::vector<std::unique_ptr<Scope>> mInnerScopes;
    


    std::vector<std::unique_ptr<type::Compound>> mCompoundTypes;
    std::vector<std::unique_ptr<type::Function>> mFunctionTypes;

    std::vector<std::unique_ptr<VarDeclNode>> mVarDecls;
    std::vector<std::unique_ptr<ConstantNode>> mConstants;
    std::vector<std::unique_ptr<VarDeclRefNode>> mVarRefs;
    std::vector<std::unique_ptr<FnNode>> mFunctions;
    std::vector<std::unique_ptr<CallNode>> mFnCalls;
    std::vector<std::unique_ptr<AssignNode>> mAssignments;
    std::vector<std::unique_ptr<RelOpNode>> mComparisons;
    std::vector<std::unique_ptr<CompoundStmtNode>> mCompoundStmts;
    std::vector<OPNode> mNodes;


};
