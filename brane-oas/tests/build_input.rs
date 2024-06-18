mod common;

use anyhow::Result;

#[test]
fn param_count_matches_function_param_count() -> Result<()> {
    let (function, _) = common::build_oas_function_param("/param-count/{1}", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 1);

    let (function, _) = common::build_oas_function_param("/param-count/{1}/{2}", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 2);

    let (function, _) = common::build_oas_function_param("/param-count/{1}/{2}/{3}", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 3);

    // For >=4 parameters, the parameters are grouped into a input type.
    let (function, types) = common::build_oas_function_param("/param-count/{1}/{2}/{3}/{4}", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 4);
    assert_eq!(types.len(), 0);

    Ok(())
}

#[test]
fn param_locations_are_irrelevant() -> Result<()> {
    let (function, types) = common::build_oas_function_param("/param-locations/{1}", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 4);
    assert_eq!(types.len(), 0);

    Ok(())
}

#[test]
fn param_required_is_preserved() -> Result<()> {
    let (function, _) = common::build_oas_function_param("/param-required", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 2);

    assert!(function.parameters.iter().filter(|p| p.name == "1").any(|p| !p.optional.unwrap()));

    assert!(function.parameters.iter().filter(|p| p.name == "2").any(|p| p.optional.unwrap()));

    // For >=4 parameters, the parameters are grouped into a input type.
    let (function, types) = common::build_oas_function_param("/param-required-count-4", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 4);
    assert_eq!(types.len(), 0);

    Ok(())
}

#[test]
fn body_none_ignored() -> Result<()> {
    let (function, _) = common::build_oas_function_body("/body-none", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 0);

    Ok(())
}

#[test]
fn body_empty_ignored() -> Result<()> {
    let (function, _) = common::build_oas_function_body("/body-empty", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 0);

    Ok(())
}

#[test]
fn body_object2props_2params() -> Result<()> {
    let (function, _) = common::build_oas_function_body("/body-object-2", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 2);

    Ok(())
}

#[test]
fn body_object4props_1param() -> Result<()> {
    let (function, types) = common::build_oas_function_body("/body-object-4", "onlyPathParameters")?;
    assert_eq!(function.parameters.len(), 4);
    assert_eq!(types.len(), 0);

    Ok(())
}

#[test]
fn servers_none_1param() -> Result<()> {
    let (function, types) = common::build_oas_function("/servers-none", "none", "servers_1_none.yml")?;
    assert_eq!(function.parameters.len(), 1);
    assert_eq!(types.len(), 0);

    Ok(())
}

#[test]
fn servers_global_0param() -> Result<()> {
    let (function, types) = common::build_oas_function("/servers-global", "global", "servers_2_global.yml")?;
    assert_eq!(function.parameters.len(), 0);
    assert_eq!(types.len(), 0);

    Ok(())
}

#[test]
fn servers_path_0param() -> Result<()> {
    let (function, types) = common::build_oas_function("/servers-path", "path", "servers_3_path.yml")?;
    assert_eq!(function.parameters.len(), 0);
    assert_eq!(types.len(), 0);

    Ok(())
}

#[test]
fn servers_operation_0param() -> Result<()> {
    let (function, types) = common::build_oas_function("/servers-operation", "operation", "servers_4_operation.yml")?;
    assert_eq!(function.parameters.len(), 0);
    assert_eq!(types.len(), 0);

    Ok(())
}
