using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace ViceCode.Analyzers.Rules
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class FullyQualifiedNameAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "VC0003";

        private const string Category = "Usage";

        private static readonly LocalizableString Title = "Avoid fully qualified name";
        private static readonly LocalizableString MessageFormat = "Method contains fully qualified name.";
        private static readonly LocalizableString DescriptionCreate = string.Empty;

        private static readonly DiagnosticDescriptor Rule = new(
            DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning,
            isEnabledByDefault: true, description: DescriptionCreate);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.RegisterSyntaxNodeAction(AnalyzeVariableDeclaration, SyntaxKind.VariableDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeMemberAccessExpression, SyntaxKind.SimpleMemberAccessExpression);
        }

        private static void AnalyzeVariableDeclaration(SyntaxNodeAnalysisContext context)
        {
            VariableDeclarationSyntax variableDeclaration = (VariableDeclarationSyntax)context.Node;

            QualifiedNameSyntax qualifiedName = variableDeclaration.DescendantNodes().OfType<QualifiedNameSyntax>().FirstOrDefault();
            if (qualifiedName is null)
                return;

            ITypeSymbol nodeType = context.SemanticModel.GetTypeInfo(qualifiedName.Left).Type;

            if (nodeType is null)
                context.ReportDiagnostic(Diagnostic.Create(Rule, qualifiedName.GetLocation()));
        }

        private static void AnalyzeMemberAccessExpression(SyntaxNodeAnalysisContext context)
        {
            MemberAccessExpressionSyntax memberAccess = (MemberAccessExpressionSyntax)context.Node;

            MemberAccessExpressionSyntax innerMemberAccess = memberAccess.DescendantNodes().OfType<MemberAccessExpressionSyntax>().FirstOrDefault();
            if (innerMemberAccess is null)
                return;

            ITypeSymbol nodeType = context.SemanticModel.GetTypeInfo(innerMemberAccess.Expression).Type;

            if (nodeType is null)
                context.ReportDiagnostic(Diagnostic.Create(Rule, memberAccess.GetLocation()));
        }
    }
}
