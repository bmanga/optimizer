#include <AST/ASTNode.hpp>
#include <AST/ASTContext.hpp>

int main() {
	/*
	 * Manual test for:

	 */

	using namespace ast;
	using namespace ast::type;

	Context Ctx;

	auto S = Ctx.globalScope();

	auto *var1 = S->makeVarDecl("var1", type::UInt32{});
	auto *var2 = S->makeVarDecl("var2", type::UInt32{});

	auto *ref1 = S->makeRef(var1);
	auto *ref2 = S->makeRef(var2);
	AssignNode *Ass1 = S->assign(ref1, S->constant(22));
	AssignNode *Ass2 = S->assign(ref2, S->constant(22));

	RelOpNode *Rel = S->compare(RelOpId::EQUAL_EQUAL, ref1, ref2);

	AssignNode *Ass3 = S->assign(ref1, Rel);

	//LogicOpNode logic1 {LogicOpId::NEGATE, &var1};
}

/*

Ctx.conditional(Ctx.currentScope(),
	Ctx.openScope()
		<< Ctx.makeVar("var1", type::UInt32{})
		<< Ctx.makeVar("var2", type::UInt32{})
		<< Ctx.makeVar("var2", type::UInt32{})
		<< Ctx.assign(Ctx.var("var1"), Ctx.constant(33))
		<< Ctx.assign(Ctx.var("var2"), Ctx.constant(44))
		<< Ctx.assign(Ctx.var("res")Ctx.compare(RelOpId::EQUAL_EQUAL, Ctx.var("var1"), Ctx.var("var2"))


*/