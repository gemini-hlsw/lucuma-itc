import graphql
import os

def generate_itc_schema(observation_db_path, itc_base_path, output_path):
    """
    Generates itc.graphql by combining ObservationDB.graphql and itc_base.graphql,
    preserving all types from itc_base.graphql and used types from ObservationDB.graphql.
    """

    # 1. Read file contents
    with open(observation_db_path, 'r') as f:
        observation_db_schema_str = f.read()
    with open(itc_base_path, 'r') as f:
        itc_base_schema_str = f.read()

    # 2. Parse schemas
    observation_db_ast = graphql.language.parse(observation_db_schema_str)
    itc_base_ast = graphql.language.parse(itc_base_schema_str)

    # 3. Extract Query type from itc_base.graphql (still needed for used types)
    itc_base_query_def = None
    for definition in itc_base_ast.definitions:
        if isinstance(definition, graphql.language.ast.ObjectTypeDefinition) and definition.name.value == 'Query':
            itc_base_query_def = definition
            break

    if not itc_base_query_def:
        raise ValueError("Query type not found in itc_base.graphql")

    # 4. & 5. Find Used Types in ObservationDB.graphql (relative to itc_base Query)
    used_types = set()
    def collect_used_types(type_node):
        if isinstance(type_node, graphql.language.ast.NamedType):
            used_types.add(type_node.name.value)
        elif isinstance(type_node, (graphql.language.ast.ListType, graphql.language.ast.NonNullType)):
            collect_used_types(type_node.type)

    # Collect used types starting from Query fields in itc_base
    if itc_base_query_def.fields:
        for field in itc_base_query_def.fields:
            collect_used_types(field.type)

    # Transitive type collection (more robust)
    all_definitions_map = {def_.name.value: def_ for def_ in observation_db_ast.definitions if hasattr(def_, 'name')}

    types_to_process = list(used_types) # Start with initial used types
    processed_types = set()

    while types_to_process:
        type_name = types_to_process.pop(0)
        if type_name in processed_types or type_name not in all_definitions_map:
            continue
        processed_types.add(type_name)

        def_ = all_definitions_map[type_name]
        if isinstance(def_, (graphql.language.ast.ObjectTypeDefinition,
                             graphql.language.ast.InterfaceTypeDefinition,
                             graphql.language.ast.InputObjectTypeDefinition)):
            if def_.fields:
                for field in def_.fields:
                    collect_used_types(field.type)
                    field_type_name = graphql.language.ast.get_named_type(field.type).name.value
                    if field_type_name not in processed_types and field_type_name in all_definitions_map:
                        types_to_process.append(field_type_name)
        elif isinstance(def_, graphql.language.ast.UnionTypeDefinition):
            if def_.types:
                for union_type in def_.types:
                    union_type_name = union_type.name.value
                    if union_type_name not in processed_types and union_type_name in all_definitions_map:
                        types_to_process.append(union_type_name)
        elif isinstance(def_, graphql.language.ast.InterfaceTypeDefinition):
            if def_.interfaces:
                for interface in def_.interfaces:
                    interface_name = interface.name.value
                    if interface_name not in processed_types and interface_name in all_definitions_map:
                        types_to_process.append(interface_name)


    # 6. Combine Schemas
    combined_definitions = []

    # Add ALL definitions from itc_base.graphql
    for definition in itc_base_ast.definitions:
        combined_definitions.append(definition)

    # Add USED types from ObservationDB.graphql (avoiding duplicates from itc_base)
    itc_base_type_names = {def_.name.value for def_ in itc_base_ast.definitions if hasattr(def_, 'name')} # Names of types in itc_base
    for definition in observation_db_ast.definitions:
        if hasattr(definition, 'name'):
            if definition.name.value in used_types and definition.name.value not in itc_base_type_names: # Check if used and NOT already in itc_base
                combined_definitions.append(definition)
        elif not hasattr(definition, 'name'): # Keep schema extensions, directives, etc.
            combined_definitions.append(definition)


    # 7. Output to itc.graphql
    combined_ast = graphql.language.ast.Document(definitions=combined_definitions)
    generated_schema_str = graphql.language.print_ast(combined_ast)

    with open(output_path, 'w') as f:
        f.write(generated_schema_str)

    print(f"itc.graphql generated successfully at {output_path}")


if __name__ == "__main__":
    observation_db_graphql_path = 'modules/service/src/main/resources/graphql/ObservationDB.graphql'
    itc_base_graphql_path = 'modules/service/src/main/resources/graphql/itc_base.graphql' # Assuming you create this file
    output_graphql_path = 'modules/service/src/main/resources/graphql/itc.graphql'

    if not os.path.exists(observation_db_graphql_path) or not os.path.exists(itc_base_graphql_path):
        print("Error: ObservationDB.graphql or itc_base.graphql not found. Please create itc_base.graphql with your Query type and other base types.")
    else:
        generate_itc_schema(observation_db_graphql_path, itc_base_graphql_path, output_graphql_path)
