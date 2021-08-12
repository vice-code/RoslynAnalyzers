using System.Collections.Immutable;
using System.Data.Common;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace ViceCode.Analyzers.Rules.SqlTypeMatching
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public sealed class SqlTypeMatchingAnalyzer : DiagnosticAnalyzer
    {
        public const string SqlTypeMatchingDiagnosticId = "VC0002";

        private static readonly LocalizableString TitleCreate = new LocalizableResourceString(nameof(Resources.SqlTypeMatchingTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormatCreate = new LocalizableResourceString(nameof(Resources.SqlTypeMatchingMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString DescriptionCreate = new LocalizableResourceString(nameof(Resources.SqlTypeMatchingDescription), Resources.ResourceManager, typeof(Resources));

        private const string Category = "Usage";

        private static readonly DiagnosticDescriptor SqlTypeMatching = new DiagnosticDescriptor(
            SqlTypeMatchingDiagnosticId,
            TitleCreate,
            MessageFormatCreate,
            Category,
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: DescriptionCreate);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(SqlTypeMatching);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution(); // Разрешить этому анализатору (правилу) работать в параллельном режиме
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.RegisterSyntaxNodeAction(AnalyzeSqlTypeMatching, SyntaxKind.SimpleAssignmentExpression);
        }

        private void AnalyzeSqlTypeMatching(SyntaxNodeAnalysisContext context)
        {
            AssignmentExpressionSyntax expression = (AssignmentExpressionSyntax)context.Node;

            if (expression.Left is MemberAccessExpressionSyntax leftExpression && leftExpression.Expression is InvocationExpressionSyntax addExpression)
            {
                if (addExpression.ArgumentList.Arguments.Count < 1)
                    return;

                INamedTypeSymbol dbParameterMetaName = context.Compilation.GetTypeByMetadataName(typeof(DbParameter).FullName);
                ISymbol valueSymbol = context.SemanticModel.GetSymbolInfo(leftExpression).Symbol; // command.Parameters.Add().|Value|

                if (valueSymbol is null)
                    return;

                if (valueSymbol.IsOverride
                    && valueSymbol.Name == "Value"
                    && valueSymbol is IPropertySymbol valuePropertySymbol
                    && SymbolEqualityComparer.Default.Equals(dbParameterMetaName, valuePropertySymbol.OverriddenProperty.ContainingType)
                    && addExpression.ArgumentList.Arguments[1].Expression is MemberAccessExpressionSyntax sqlType)
                {
                    SpecialType rightExpressionType = 0;

                    if (expression.Right is IdentifierNameSyntax identifierExpression)
                        rightExpressionType = context.SemanticModel.GetTypeInfo(identifierExpression).Type.SpecialType;

                    else if (expression.Right is BinaryExpressionSyntax binaryExpression)
                    {
                        if (binaryExpression.Left is CastExpressionSyntax leftCast)
                            rightExpressionType = context.SemanticModel.GetTypeInfo(leftCast.Expression).Type.SpecialType;
                        else
                            rightExpressionType = context.SemanticModel.GetTypeInfo(binaryExpression.Left).Type.SpecialType;
                    }

                    else if (expression.Right is ExpressionSyntax expressionExpression)
                        rightExpressionType = context.SemanticModel.GetTypeInfo(expressionExpression).Type.SpecialType;

                    if (rightExpressionType is SpecialType.System_Object or SpecialType.None)
                        return;

                    if (!IsValid(rightExpressionType, sqlType.Name.Identifier.ValueText))
                    {
                        Diagnostic diagnostic = Diagnostic.Create(SqlTypeMatching, expression.GetLocation());
                        context.ReportDiagnostic(diagnostic);
                    }
                }
            }
        }

        private bool IsValid(SpecialType expressionType, string sqlType)
        {
            if ((sqlType is "Int"
                or "BigInt"
                or "SmallInt"
                or "TinyInt")
                && IsInt(expressionType))
                return true;

            if (sqlType is "Bit" && expressionType is SpecialType.System_Boolean)
                return true;

            if ((sqlType is "VarChar"
                or "Char"
                or "NChar"
                or "NText"
                or "NVarChar"
                or "Text")
                && expressionType is SpecialType.System_String)
                return true;

            if ((sqlType is "Float"
                or "Decimal"
                or "Real"
                or "Money"
                or "SmallMoney"
                ) && IsFloating(expressionType))
                return true;

            if (sqlType is "UniqueIdentifier" && expressionType is SpecialType.System_ValueType)
                return true;

            if ((sqlType is "Time"
                or "Date"
                or "DateTime"
                or "DateTime2"
                or "DateTimeOffset"
                or "SmallDateTime") && expressionType is SpecialType.System_DateTime)
                return true;

            if ((sqlType is "Image"
                or "Timestamp"
                or "VarBinary"
                or "Variant"
                or "Xml"
                or "Udt"
                or "Structured"
                ) && expressionType is SpecialType.System_Object)
                return true;

            return false;
        }

        private bool IsInt(SpecialType type) =>
            type is SpecialType.System_Int32
            or SpecialType.System_UInt16
            or SpecialType.System_Int16
            or SpecialType.System_Byte
            or SpecialType.System_UInt32
            or SpecialType.System_Int64
            or SpecialType.System_UInt64;

        private bool IsFloating(SpecialType type) =>
            type is SpecialType.System_Single
            or SpecialType.System_Double
            or SpecialType.System_Decimal;
    }
}
