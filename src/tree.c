#include "tree.h"
#include "symtab.h"

char* base_type_to_string(base_data_type_t bt){
        switch(bt){
        case INT_TYPE:
                return "INT";
        case FLOAT_TYPE:
                return "FLOAT";
        case BOOL_TYPE:
                return "BOOL";
        case VOID_TYPE:
                return "VOID";
        case STRING_TYPE:
                return "STRING";
        case ARRAY_TYPE:
                return "ARRAY";
        default:
                return "";
        }
}

// TODO
char* type_to_string(data_type_t type){
        switch(type.base_type){


                case ARRAY_TYPE:
                {
                        char* buffer = malloc(200);
                        char index_buffer[10];

                        sprintf(buffer, "ARRAY(%s)", base_type_to_string(type.array_type));
                        for(int i = 0; i < type.n_dimensions; i++){
                                sprintf(index_buffer, "[%d]", type.dimensions[i]);
                                strcat(buffer, index_buffer);
                        }
                        return buffer;
                }
                default:
                        return base_type_to_string(type.base_type);
        }
}

// Print a node and all children recursively. 
void node_print ( FILE *output, node_t *root, int nesting )
{
	printf("Node print is called, indeed: output ptr: '%p', root ptr: '%p'\n", output,  root);
    if ( root != NULL )
    {
        // Print 'nesting' number of ' ' before node text description
        fprintf ( output, "%*c%s", nesting, ' ', root->nodetype.text );

        // Print data type
        fprintf(output, "(%s)", type_to_string(root->data_type));
        
        // Print the value of constants
        if ( root->nodetype.index == CONSTANT || root->nodetype.index == INDEX ) {
        	switch (root->data_type.base_type)
        	{
            	case INT_TYPE:
            		fprintf ( output, "(%d)", root->int_const );
            		break;
            	case FLOAT_TYPE:
            		fprintf ( output, "(%f)", root->float_const );
            		break;
            	case BOOL_TYPE:
            		fprintf ( output, "(%s)", root->bool_const ? "True" : "False" );
            		break;
            	case STRING_TYPE:
            		fprintf ( output, "(%s)", root->string_const );
            		break;

            	default:
            		fprintf ( output, "(ERROR: No type set for constant/index)" );
            		break;
            }
        }

        // Print the label of nodes where it is set,
        if(root->label != NULL){
        	fprintf ( output, "(\"%s\")", root->label );
        }

        // Print expression type where it is set

        if(root->expression_type.text != NULL){
        	fprintf ( output, "(%s)", root->expression_type.text);
        }


        fputc ( '\n', output );
        for ( int i=0; i<root->n_children; i++ )
            node_print ( output, root->children[i], nesting+1 );
    }
    else
        fprintf ( output, "%*c%p\n", nesting, ' ', root );
}


// Print the symbol table entries attached to a node and all children recursively
void node_print_entries ( FILE *output, node_t *root, int nesting )
{
    if ( root != NULL )
    {
        // Print 'nesting' number of ' ' before node text description
        fprintf ( output, "%*c%s", nesting, ' ', root->nodetype.text );
        if ( root->nodetype.index == CONSTANT ) {
            if ( root->label != NULL )
                fprintf ( output, "(\"%s\"), ", root->label );
            
        }
        if ( root->nodetype.index == VARIABLE || root->nodetype.index == FUNCTION )
        {
            if ( root->label != NULL )
                fprintf ( output, ", (\"%s\")", root->label );
            else
                fprintf ( output, "%p", NULL );
            
            if ( root->entry != NULL ) {
                fprintf ( output, ", depth: %d, stack_offset: %d", root->entry->depth, root->entry->stack_offset );
                    if ( root->entry->label != NULL )
                        fprintf ( output, ", entry label: \"%s\"", root->entry->label );
            }
            else
                fprintf ( output, ", %p", NULL );
        }
        fputc ( '\n', output );
        for ( int i=0; i<root->n_children; i++ )
            node_print_entries ( output, root->children[i], nesting+1 );
    }
    else
        fprintf ( output, "%*c%p\n", nesting, ' ', root );
}

node_t *node_init ( nodetype_t type,
		char* label,
		base_data_type_t base_type,
		expression_type_t expression_type,
		int n_children,
		va_list child_list )
{
    node_t *init = (node_t *) malloc(sizeof(node_t));
    init->nodetype = type;
    init->label = label;
    init->data_type.base_type = base_type;
    init->expression_type = expression_type;
    init->n_children = n_children;
    if (n_children > 0)
    	init->children = (node_t **) malloc(sizeof(node_t *) * n_children);
    else 
    	init->children = NULL;
    for ( int x = 0; x < n_children; ++x )        
    {
		node_t *child = va_arg(child_list, node_t *);
        init->children[x] = child;
    }
}
// 55 correct, 37 tests => 111 tests

void node_finalize ( node_t *discard )
{
	if (discard->n_children > 0)
    	free(discard->children);
    free(discard);
}


void destroy_subtree ( FILE *output, node_t *discard )
{
    for (int i = 0; i < discard->n_children; ++i)
        destroy_subtree(output, discard->children[i]);
    node_finalize(discard);
}
