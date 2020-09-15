using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Data;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace ViceCode.Analyzers
{
	[ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(DataRowConstructorCodeFixProvider)), Shared]
	public class DataRowConstructorCodeFixProvider : CodeFixProvider
	{
		// <SnippetCodeFixTitle>
		private const string titleCreate = "Generate constructor(DataRow row)";
		private const string titleUpdate = "Update constructor(DataRow row)";
		// </SnippetCodeFixTitle>

		public sealed override ImmutableArray<string> FixableDiagnosticIds
		{
			get { return ImmutableArray.Create(DataRowConstructorAnalyzer.CreateDataRowConstructorDiagnosticId, DataRowConstructorAnalyzer.UpdateDataRowConstructorDiagnosticId); }
		}

		public sealed override FixAllProvider GetFixAllProvider()
		{
			// See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
			return WellKnownFixAllProviders.BatchFixer;
		}

		public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
		{
			var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken);

			var diagnostic = context.Diagnostics.First();
			var diagnosticSpan = diagnostic.Location.SourceSpan;

			// Find the type declaration identified by the diagnostic.
			var classDeclation = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ClassDeclarationSyntax>().First();

			if (diagnostic.Id == DataRowConstructorAnalyzer.CreateDataRowConstructorDiagnosticId)
			{
				// Register a code action that will invoke the fix.
				context.RegisterCodeFix(
					CodeAction.Create(
						title: titleCreate,
						createChangedDocument: ct => CreateDataRowConstructor(context.Document, classDeclation, ct),
						equivalenceKey: titleCreate),
					diagnostic);
				return;
			}

			var constructorDeclation = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ConstructorDeclarationSyntax>().First();

			// Register a code action that will invoke the fix.
			context.RegisterCodeFix(
				CodeAction.Create(
					title: titleUpdate,
					createChangedDocument: ct => UpdateDataRowConstructor(context.Document, classDeclation, constructorDeclation, ct),
					equivalenceKey: titleUpdate),
				diagnostic);
		}

		private async Task<Document> UpdateDataRowConstructor(Document document, ClassDeclarationSyntax classDeclaration, ConstructorDeclarationSyntax constructorDeclaration, CancellationToken ct)
		{
			var unsetProperties = Helper.GetClassUnsetProperties(classDeclaration, constructorDeclaration);

			SyntaxTriviaList leadingTrivia;
			SyntaxList<StatementSyntax> statements;

			if (constructorDeclaration.Body is null)
			{
				leadingTrivia = constructorDeclaration.GetLeadingTrivia();
			}
			else
			{
				statements = constructorDeclaration.Body.Statements;
				leadingTrivia = statements.Any() ? statements.First().GetLeadingTrivia() : constructorDeclaration.GetLeadingTrivia();
			}

			foreach (var property in unsetProperties)
			{
				var assigment = CreatePropertyAssigmentExpression(constructorDeclaration.ParameterList.Parameters[0], property);

				statements = statements.Add(SyntaxFactory.ExpressionStatement(assigment).WithLeadingTrivia(leadingTrivia));
			}

			// Копируем параметры конструктора, заменяя только Body.
			var updatedConstructor = SyntaxFactory.ConstructorDeclaration(constructorDeclaration.AttributeLists, constructorDeclaration.Modifiers, constructorDeclaration.Identifier, constructorDeclaration.ParameterList, constructorDeclaration.Initializer, SyntaxFactory.Block(statements));

			// Replace the old constructor declaration with the new local declaration.
			var oldRoot = await document.GetSyntaxRootAsync(ct);
			var newRoot = oldRoot.ReplaceNode(constructorDeclaration, updatedConstructor);

			// Return document with transformed tree.
			return document.WithSyntaxRoot(newRoot);
		}

		private async Task<Document> CreateDataRowConstructor(Document document, ClassDeclarationSyntax classDeclaration, CancellationToken ct)
		{
			// attributes;
			var attributes = new SyntaxList<AttributeListSyntax>();

			// Modifiers;
			const string publicText = "public ";
			var publicModifier = SyntaxFactory.Identifier(SyntaxTriviaList.Empty, SyntaxKind.PublicKeyword, publicText, publicText, SyntaxTriviaList.Empty);
			var modifiers = new SyntaxTokenList(publicModifier);

			// Identifier;
			var identifier = SyntaxFactory.Identifier(SyntaxTriviaList.Empty, SyntaxKind.PublicKeyword, classDeclaration.Identifier.Text, classDeclaration.Identifier.ValueText, SyntaxTriviaList.Empty)/*.WithLeadingTrivia().WithTrailingTrivia()*/;

			// ParameterList;
			var parameterName = SyntaxFactory.Identifier("row").WithTrailingTrivia().WithLeadingTrivia();
			var parameterType = SyntaxFactory.IdentifierName(typeof(DataRow).Name);
			var parameter = SyntaxFactory.Parameter(new SyntaxList<AttributeListSyntax>(), new SyntaxTokenList(), parameterType, parameterName, null);
			var parameters = SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(new[] { parameter }));

			// Body block
			var properties = classDeclaration.Members.OfType<PropertyDeclarationSyntax>().ToList();
			var statements = new List<StatementSyntax>(properties.Count);

			foreach (var property in properties)
			{
				statements.Add(SyntaxFactory.ExpressionStatement(CreatePropertyAssigmentExpression(parameter, SyntaxFactory.PropertyDeclaration(property.Type, property.Identifier))));
			}

			var block = SyntaxFactory.Block(statements);

			var constructor = SyntaxFactory.ConstructorDeclaration(attributes, modifiers, identifier, parameters, null, block);

			var fpt = properties.First().GetLeadingTrivia().Where(t => t.IsKind(SyntaxKind.WhitespaceTrivia)).First();

			var newClassDexlaration = classDeclaration.InsertNodesBefore(classDeclaration.Members.FirstOrDefault(), new[] { constructor.WithLeadingTrivia(fpt) });

			var root = await document.GetSyntaxRootAsync(ct);

			CompilationUnitSyntax newRoot = (CompilationUnitSyntax)root.ReplaceNode(classDeclaration, newClassDexlaration);

			bool usingFinded = false;
			foreach (var usingDirective in newRoot.Usings)
			{
				if (usingDirective.Name.ToString().Equals("System.Data"))
				{
					usingFinded = true;
					break;
				}
			}

			if (!usingFinded)
			{
				newRoot = newRoot.AddUsings(SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName("System.Data")));
			}

			return document.WithSyntaxRoot(newRoot);
		}

		private static AssignmentExpressionSyntax CreatePropertyAssigmentExpression(ParameterSyntax parameter, PropertyDeclarationSyntax property)
		{
			var left = SyntaxFactory.IdentifierName(property.Identifier);

			// GenericName
			var typeSeparatedList = SyntaxFactory.SeparatedList(new[] { property.Type });                                                       // {property type}
			var typeArgumentList = SyntaxFactory.TypeArgumentList(typeSeparatedList);                                                           // <{property type}>
			var generic = SyntaxFactory.Identifier("Field");                                                                                    // Field
			var genericName = SyntaxFactory.GenericName(generic, typeArgumentList);                                                             // Field<{Property type}>

			var rowParam = SyntaxFactory.IdentifierName(parameter.Identifier);                                                                  // row
			var memberAccess = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, rowParam, genericName);            // add Field<{Property type}> to row
			var text = $"\"{property.Identifier.Text}\"";                                                                                       // ( "{Property name}")
			var argName = SyntaxFactory.Token(SyntaxTriviaList.Empty, SyntaxKind.StringLiteralToken, text, text, SyntaxTriviaList.Empty);
			var argument = SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, argName));
			var separatedList = SyntaxFactory.SeparatedList(new[] { argument });
			var argumentList = SyntaxFactory.ArgumentList(separatedList);

			var right = SyntaxFactory.InvocationExpression(memberAccess, argumentList);                                                         // row.Field<{property type}>("{property name}");
			return SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, left, right);
		}
	}
}
