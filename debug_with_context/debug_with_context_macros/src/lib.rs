use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Field};


fn gen_field(field: &Field, is_struct : bool) -> proc_macro2::TokenStream {
    let field_name = match &field.ident {
        Some(i) => i.clone(),
        None => todo!(), // TODO : handle struct tuples ?
    };

    let field_name_str= field_name.to_string();
    let field_name_lit = syn::LitStr::new(&field_name_str, proc_macro2::Span::call_site());

    let obj_access = if is_struct {
        quote! {
                self. #field_name
            }
    } else {
        quote! {
            #field_name
        }
    };

    quote! {
        .field_with(#field_name_lit,  |fmt| {
            #obj_access .fmt_with_context(fmt, context)
        })    
    }

    
}

#[proc_macro_derive(DebugWithContext, attributes(debug_context))]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, attrs, vis: _, generics, data } = parse_macro_input!(input);
    let mut context_struct = None;
    for attr in  attrs {
        if attr.path().is_ident("debug_context") {
            attr.parse_nested_meta(|meta| {
                context_struct = Some(meta.path.get_ident().expect("Expected an identifier for the debug context struct").clone());
                Ok(())
            }).unwrap();
        }
    }
    

    let context_struct = match context_struct {
        Some(cs) => cs,
        None => {
            return syn::Error::new_spanned(ident, "Missing #[debug_context(...)] attribute").to_compile_error().into();
        }
    };

    let generic_param_types = generics.type_params().map(|t| t.ident.clone()).collect::<Vec<_>>();

    let mut generic_quote = None;

    if !generic_param_types.is_empty() {
        generic_quote = Some(quote! {
            <#(#generic_param_types,)*>
        });
    }

    let ident_str = ident.to_string();
    let ident_lit = syn::LitStr::new(&ident_str, proc_macro2::Span::call_site());

    // TODO : add support for tuple enums and structs
    let fmt_code = match data {
        Data::Enum(e) => {
            let variants = e.variants.iter().map(|v|{
                let variant_name = &v.ident;
                let variant_name_str= variant_name.to_string();
                let variant_name_lit = syn::LitStr::new(&variant_name_str, proc_macro2::Span::call_site());
                let variant_field_names = v.fields.iter().map(|f| f.ident.as_ref().unwrap().clone()).collect::<Vec<_>>();
                let variant_fields = v.fields.iter().map(|f| gen_field(f, false)).collect::<Vec<_>>();
                quote! {
                    Self:: #variant_name { #(#variant_field_names,)* } => f.debug_struct(#variant_name_lit)
                                        #(#variant_fields)* .finish() ,
                }
            }).collect::<Vec<_>>();

            quote! {
                match self {
                    #(#variants)*
                }
            }
            
        },
        Data::Struct(s) => {
            let fields = s.fields.iter().map(|f| gen_field(f, true)).collect::<Vec<_>>();

            quote! {
                f.debug_struct(#ident_lit)
                #(#fields)*
                .finish()
            }
        },
        Data::Union(_) => panic!("Union are not supported for now"),
    };

    let output = quote! {
        impl #generic_quote DebugWithContext<#context_struct> for #ident #generic_quote {
            fn fmt_with_context(&self, f: &mut ::std::fmt::Formatter, context: &#context_struct) -> ::std::fmt::Result {
                #fmt_code
            }
        }
    };
    output.into()
}