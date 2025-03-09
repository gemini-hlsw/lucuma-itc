import * as fs from 'fs';
import { parse, print, ObjectTypeDefinitionNode, DefinitionNode, NamedTypeNode, ListTypeNode, NonNullTypeNode, getNamedType, UnionTypeDefinitionNode, InterfaceTypeDefinitionNode } from 'graphql';

function generateItcSchema(observationDbPath: string, itcBasePath: string, outputPath: string): void {
    /**
     * Generates itc.graphql by combining ObservationDB.graphql and itc_base.graphql,
     * preserving all types from itc_base.graphql and used types from ObservationDB.graphql.
     */

    // 1. Read file contents
    const observationDbSchemaStr: string = fs.readFileSync(observationDbPath, 'utf-8');
    const itcBaseSchemaStr: string = fs.readFileSync(itcBasePath, 'utf-8');

    // 2. Parse schemas
    const observationDbAst = parse(observationDbSchemaStr);
    const itcBaseAst = parse(itcBaseSchemaStr);

    // 3. Extract Query type from itc_base.graphql
    let itcBaseQueryDef: ObjectTypeDefinitionNode | null = null;
    for (const definition of itcBaseAst.definitions) {
        if (definition.kind === 'ObjectTypeDefinition' && definition.name.value === 'Query') {
            itcBaseQueryDef = definition;
            break;
        }
    }

    if (!itcBaseQueryDef) {
        throw new Error("Query type not found in itc_base.graphql");
    }

    // 4. & 5. Find Used Types in ObservationDB.graphql (relative to itc_base Query)
    const usedTypes: Set<string> = new Set();
    function collectUsedTypes(typeNode: NamedTypeNode | ListTypeNode | NonNullTypeNode | undefined): void {
        if (!typeNode) return;
        if (typeNode.kind === 'NamedType') {
            usedTypes.add(typeNode.name.value);
        } else if (typeNode.kind === 'ListType' || typeNode.kind === 'NonNullType') {
            collectUsedTypes(typeNode.type);
        }
    }

    // Collect used types starting from Query fields in itc_base
    if (itcBaseQueryDef.fields) {
        for (const field of itcBaseQueryDef.fields) {
            collectUsedTypes(field.type);
        }
    }

    // Transitive type collection (more robust)
    const allDefinitionsMap: { [typeName: string]: DefinitionNode } = {};
    for (const def_ of observationDbAst.definitions) {
        if ('name' in def_ && def_.name) {
            allDefinitionsMap[def_.name.value] = def_;
        }
    }

    const typesToProcess: string[] = Array.from(usedTypes); // Start with initial used types
    const processedTypes: Set<string> = new Set();

    while (typesToProcess.length > 0) {
        const typeName = typesToProcess.shift();
        if (!typeName || processedTypes.has(typeName) || !(typeName in allDefinitionsMap)) {
            continue;
        }
        processedTypes.add(typeName);

        const def_ = allDefinitionsMap[typeName];
        if (def_.kind === 'ObjectTypeDefinition' ||
            def_.kind === 'InterfaceTypeDefinition' ||
            def_.kind === 'InputObjectTypeDefinition') {
            if (def_.fields) {
                for (const field of def_.fields) {
                    collectUsedTypes(field.type);
                    const fieldTypeName = getNamedType(field.type).name.value;
                    if (!processedTypes.has(fieldTypeName) && fieldTypeName in allDefinitionsMap) {
                        typesToProcess.push(fieldTypeName);
                    }
                }
            }
        } else if (def_.kind === 'UnionTypeDefinition') {
            const unionDef = def_ as UnionTypeDefinitionNode; // Explicit cast for type narrowing
            if (unionDef.types) {
                for (const unionType of unionDef.types) {
                    const unionTypeName = unionType.name.value;
                    if (!processedTypes.has(unionTypeName) && unionTypeName in allDefinitionsMap) {
                        typesToProcess.push(unionTypeName);
                    }
                }
            }
        } else if (def_.kind === 'InterfaceTypeDefinition') {
            const interfaceDef = def_ as InterfaceTypeDefinitionNode; // Explicit cast for type narrowing
            if (interfaceDef.interfaces) {
                for (const interfaceNode of interfaceDef.interfaces) {
                    const interfaceName = interfaceNode.name.value;
                    if (!processedTypes.has(interfaceName) && interfaceName in allDefinitionsMap) {
                        typesToProcess.push(interfaceName);
                    }
                }
            }
        }
    }


    // 6. Combine Schemas
    const combinedDefinitions: DefinitionNode[] = [];

    // Add ALL definitions from itc_base.graphql
    for (const definition of itcBaseAst.definitions) {
        combinedDefinitions.push(definition);
    }

    // Add USED types from ObservationDB.graphql (avoiding duplicates from itc_base)
    const itcBaseTypeNames: Set<string> = new Set(itcBaseAst.definitions.filter(def => 'name' in def && def.name).map(def => (def as any).name.value)); // Names of types in itc_base
    for (const definition of observationDbAst.definitions) {
        if ('name' in definition && definition.name) {
            if (usedTypes.has(definition.name.value) && !itcBaseTypeNames.has(definition.name.value)) { // Check if used and NOT already in itc_base
                combinedDefinitions.push(definition);
            }
        } else { // Keep schema extensions, directives, etc.
            combinedDefinitions.push(definition);
        }
    }


    // 7. Output to itc.graphql
    const combinedAst = { kind: 'Document', definitions: combinedDefinitions } as graphql.ASTNode; // Type assertion for ASTNode
    const generatedSchemaStr: string = print(combinedAst);

    fs.writeFileSync(outputPath, generatedSchemaStr);

    console.log(`itc.graphql generated successfully at ${outputPath}`);
}

if (__filename === process.argv[1]) {
    const observationDbGraphqlPath = 'modules/service/src/main/resources/graphql/ObservationDB.graphql';
    const itcBaseGraphqlPath = 'modules/service/src/main/resources/graphql/itc_base.graphql'; // Assuming you create this file
    const outputGraphqlPath = 'modules/service/src/main/resources/graphql/itc.graphql';

    if (!fs.existsSync(observationDbGraphqlPath) || !fs.existsSync(itcBaseGraphqlPath)) {
        console.error("Error: ObservationDB.graphql or itc_base.graphql not found. Please create itc_base.graphql with your Query type and other base types.");
    } else {
        generateItcSchema(observationDbGraphqlPath, itcBaseGraphqlPath, outputGraphqlPath);
    }
}
