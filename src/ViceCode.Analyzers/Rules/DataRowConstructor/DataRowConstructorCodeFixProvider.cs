using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Data;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using ViceCode.Analyzers.Utils;

namespace ViceCode.Analyzers.Rules.DataRowConstructor
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(DataRowConstructorCodeFixProvider)), Shared]
    public class DataRowConstructorCodeFixProvider : CodeFixProvider
    {
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
            SyntaxNode root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            Diagnostic diagnostic = context.Diagnostics.First();
            TextSpan diagnosticSpan = diagnostic.Location.SourceSpan;

            // Find the type declaration identified by the diagnostic.
            TypeDeclarationSyntax typeDeclation = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();

            if (diagnostic.Id == DataRowConstructorAnalyzer.CreateDataRowConstructorDiagnosticId)
            {
                string title = string.Format("Generate {0}(DataRow row) constructor", typeDeclation.Identifier.ValueText);
                // Register a code action that will invoke the fix.
                context.RegisterCodeFix(
                    CodeAction.Create(
                        title: title,
                        createChangedDocument: ct => CreateDataRowConstructor(context.Document, typeDeclation, ct),
                        equivalenceKey: title),
                    diagnostic);
                return;
            }

            ConstructorDeclarationSyntax constructorDeclation = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ConstructorDeclarationSyntax>().First();

            string key = string.Format("Update {0}(DataRow row) constructor", typeDeclation.Identifier.ValueText);
            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: key,
                    createChangedDocument: ct => UpdateDataRowConstructor(context.Document, typeDeclation, constructorDeclation, ct),
                    equivalenceKey: key),
                diagnostic);
        }

        private async Task<Document> UpdateDataRowConstructor(Document document, TypeDeclarationSyntax typeDeclaration, ConstructorDeclarationSyntax constructorDeclaration, CancellationToken ct)
        {
            List<PropertyDeclarationSyntax> unsetProperties = Helper.GetClassUnsetProperties(typeDeclaration, constructorDeclaration, true);

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

            foreach (PropertyDeclarationSyntax property in unsetProperties)
            {
                AssignmentExpressionSyntax assigment = CreatePropertyAssigmentExpression(constructorDeclaration.ParameterList.Parameters[0], property);

                statements = statements.Add(SyntaxFactory.ExpressionStatement(assigment).WithLeadingTrivia(leadingTrivia));
            }

            // Копируем параметры конструктора, заменяя только Body.
            ConstructorDeclarationSyntax updatedConstructor = SyntaxFactory.ConstructorDeclaration(constructorDeclaration.AttributeLists, constructorDeclaration.Modifiers, constructorDeclaration.Identifier, constructorDeclaration.ParameterList, constructorDeclaration.Initializer, SyntaxFactory.Block(statements));

            // Replace the old constructor declaration with the new local declaration.
            SyntaxNode oldRoot = await document.GetSyntaxRootAsync(ct);
            SyntaxNode newRoot = oldRoot.ReplaceNode(constructorDeclaration, updatedConstructor);

            // Return document with transformed tree.
            return document.WithSyntaxRoot(newRoot);
        }

        private async Task<Document> CreateDataRowConstructor(Document document, TypeDeclarationSyntax typeDeclaration, CancellationToken ct)
        {
            // attributes;
            SyntaxList<AttributeListSyntax> attributes = new SyntaxList<AttributeListSyntax>();

            // Modifiers;
            const string publicText = "public ";
            SyntaxToken publicModifier = SyntaxFactory.Identifier(SyntaxTriviaList.Empty, SyntaxKind.PublicKeyword, publicText, publicText, SyntaxTriviaList.Empty);
            SyntaxTokenList modifiers = new SyntaxTokenList(publicModifier);

            // Identifier;
            SyntaxToken identifier = SyntaxFactory.Identifier(SyntaxTriviaList.Empty, SyntaxKind.PublicKeyword, typeDeclaration.Identifier.Text, typeDeclaration.Identifier.ValueText, SyntaxTriviaList.Empty)/*.WithLeadingTrivia().WithTrailingTrivia()*/;

            // ParameterList;
            SyntaxToken parameterName = SyntaxFactory.Identifier("row").WithTrailingTrivia().WithLeadingTrivia();
            IdentifierNameSyntax parameterType = SyntaxFactory.IdentifierName(typeof(DataRow).Name);
            ParameterSyntax parameter = SyntaxFactory.Parameter(new SyntaxList<AttributeListSyntax>(), new SyntaxTokenList(), parameterType, parameterName, null);
            ParameterListSyntax parameters = SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(new[] { parameter }));

            // Body block
            List<PropertyDeclarationSyntax> properties = typeDeclaration.Members.OfType<PropertyDeclarationSyntax>().ToList();
            List<StatementSyntax> statements = new List<StatementSyntax>(properties.Count);

            foreach (PropertyDeclarationSyntax property in properties)
            {
                if (property.AccessorList.Accessors.Count == 1 && property.AccessorList.Accessors[0].Kind() == SyntaxKind.GetAccessorDeclaration)
                    continue;

                statements.Add(SyntaxFactory.ExpressionStatement(CreatePropertyAssigmentExpression(parameter, SyntaxFactory.PropertyDeclaration(property.Type, property.Identifier))));
            }

            BlockSyntax block = SyntaxFactory.Block(statements);

            ConstructorDeclarationSyntax constructor = SyntaxFactory.ConstructorDeclaration(attributes, modifiers, identifier, parameters, null, block);

            SyntaxTrivia fpt = properties.First().GetLeadingTrivia().Where(t => t.IsKind(SyntaxKind.WhitespaceTrivia)).First();

            TypeDeclarationSyntax newClassDexlaration = typeDeclaration.InsertNodesBefore(typeDeclaration.Members.FirstOrDefault(), new[] { constructor.WithLeadingTrivia(fpt) });

            SyntaxNode root = await document.GetSyntaxRootAsync(ct);

            CompilationUnitSyntax newRoot = (CompilationUnitSyntax)root.ReplaceNode(typeDeclaration, newClassDexlaration);

            bool usingFinded = false;
            foreach (UsingDirectiveSyntax usingDirective in newRoot.Usings)
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
            IdentifierNameSyntax left = SyntaxFactory.IdentifierName(property.Identifier);

            // GenericName
            SeparatedSyntaxList<TypeSyntax> typeSeparatedList = SyntaxFactory.SeparatedList(new[] { property.Type });                                                       // {property type}
            TypeArgumentListSyntax typeArgumentList = SyntaxFactory.TypeArgumentList(typeSeparatedList);                                                           // <{property type}>
            SyntaxToken generic = SyntaxFactory.Identifier("Field");                                                                                    // Field
            GenericNameSyntax genericName = SyntaxFactory.GenericName(generic, typeArgumentList);                                                             // Field<{Property type}>

            IdentifierNameSyntax rowParam = SyntaxFactory.IdentifierName(parameter.Identifier);                                                                  // row
            MemberAccessExpressionSyntax memberAccess = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, rowParam, genericName);            // add Field<{Property type}> to row
            string text = $"\"{property.Identifier.Text}\"";                                                                                       // ( "{Property name}")
            SyntaxToken argName = SyntaxFactory.Token(SyntaxTriviaList.Empty, SyntaxKind.StringLiteralToken, text, text, SyntaxTriviaList.Empty);
            ArgumentSyntax argument = SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, argName));
            SeparatedSyntaxList<ArgumentSyntax> separatedList = SyntaxFactory.SeparatedList(new[] { argument });
            ArgumentListSyntax argumentList = SyntaxFactory.ArgumentList(separatedList);

            InvocationExpressionSyntax right = SyntaxFactory.InvocationExpression(memberAccess, argumentList);                                                         // row.Field<{property type}>("{property name}");
            return SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, left, right);
        }
    }
}
