using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using ViceCode.Analyzers.Utils;

namespace ViceCode.Analyzers.Rules
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class DataRowConstructorAnalyzer : DiagnosticAnalyzer
    {
        public const string CreateDataRowConstructorDiagnosticId = "VC0000";
        public const string UpdateDataRowConstructorDiagnosticId = "VC0001";

        private const string Category = "Usage";

        private static readonly LocalizableString TitleCreate = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormatCreate = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString DescriptionCreate = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString TitleUpdate = new LocalizableResourceString(nameof(Resources.AnalyzerTitleUpdate), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormatUpdate = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormatUpdate), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString DescriptionUpdate = new LocalizableResourceString(nameof(Resources.AnalyzerDescriptionUpdate), Resources.ResourceManager, typeof(Resources));

        private static readonly DiagnosticDescriptor CreateRule = new(
            CreateDataRowConstructorDiagnosticId, TitleCreate, MessageFormatCreate, Category, DiagnosticSeverity.Info,
            isEnabledByDefault: true, description: DescriptionCreate);

        private static readonly DiagnosticDescriptor UpdateRule = new(
            UpdateDataRowConstructorDiagnosticId, TitleUpdate, MessageFormatUpdate, Category, DiagnosticSeverity.Info,
            isEnabledByDefault: true, description: DescriptionUpdate);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(CreateRule, UpdateRule);

        public override void Initialize(AnalysisContext context)
        {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            // <SnippetRegisterNodeAction>
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterSyntaxNodeAction(AnalyzeNode<ClassDeclarationSyntax>, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeNode<RecordDeclarationSyntax>, SyntaxKind.RecordDeclaration);
            // </SnippetRegisterNodeAction>
        }

        private void AnalyzeNode<TNode>(SyntaxNodeAnalysisContext context) where TNode : TypeDeclarationSyntax
        {
            TNode typeDeclaration = (TNode)context.Node;
            SemanticModel semanticModel = context.SemanticModel;

            Lazy<INamedTypeSymbol> dataRowNamedTypeSymbol = new(() => context.Compilation.GetTypeByMetadataName(typeof(DataRow).FullName));

            foreach (MemberDeclarationSyntax member in typeDeclaration.Members)
            {
                if (member.Kind() != SyntaxKind.ConstructorDeclaration)
                    continue;

                ConstructorDeclarationSyntax constructorDeclaration = (ConstructorDeclarationSyntax)member;
                SeparatedSyntaxList<ParameterSyntax> parameters = constructorDeclaration.ParameterList.Parameters;
                if (parameters.Count != 1)
                {
                    // Нас интересует только один параметр в конструкторе.
                    continue;
                }

                IParameterSymbol paramSymbol = semanticModel.GetDeclaredSymbol(parameters[0]);
                ITypeSymbol paramTypeSymbol = paramSymbol.Type;

                if (!SymbolEqualityComparer.Default.Equals(dataRowNamedTypeSymbol.Value, paramTypeSymbol))
                {
                    // Единственный параметр конструктора не является DataRow, ищем дальше
                    continue;
                }

                List<PropertyDeclarationSyntax> listProperties = Helper.GetClassUnsetProperties(typeDeclaration, constructorDeclaration, ignoreGetOnly: true); // Свойства, которые не заданы в конструкторе.

                if (listProperties.Count == 0)
                {
                    // Список пуст - нечего добавлять
                    return;
                }

                context.ReportDiagnostic(Diagnostic.Create(UpdateRule, constructorDeclaration.Identifier.GetLocation()));
                return;
            }

            IEnumerable<PropertyDeclarationSyntax> rawProperties = typeDeclaration.Members.OfType<PropertyDeclarationSyntax>(); // Берём только свойства.
            List<PropertyDeclarationSyntax> properties = rawProperties.ToList();

            foreach (PropertyDeclarationSyntax property in rawProperties)
            {
                if (property.AccessorList.Accessors.Count == 1 && property.AccessorList.Accessors[0].Kind() == SyntaxKind.GetAccessorDeclaration)
                    properties.Remove(property);
            }

            if (properties.Count == 0)
                return; // нечего устанавливать.

            context.ReportDiagnostic(Diagnostic.Create(CreateRule, typeDeclaration.Identifier.GetLocation()));
        }
    }
}
