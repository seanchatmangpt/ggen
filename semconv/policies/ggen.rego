package before_resolution

import rego.v1

# ggen group ID prefixes — scope all rules to ggen registry only.
ggen_prefixes := {
  "registry.ggen.a2a", "registry.ggen.llm", "registry.ggen.mcp",
  "registry.ggen.yawl", "registry.ggen.pipeline", "registry.ggen.error",
  "span.ggen.a2a", "span.ggen.llm", "span.ggen.mcp",
  "span.ggen.yawl", "span.ggen.pipeline", "span.ggen.error",
}

is_ggen_group(group) if {
  some prefix in ggen_prefixes
  startswith(group.id, prefix)
}

# ============================================================
# Rule 1: All ggen groups must declare stability.
# ============================================================
deny contains violation if {
  group := input.groups[_]
  is_ggen_group(group)
  not group.stability
  violation := {
    "id": "ggen_missing_stability",
    "level": "violation",
    "message": sprintf("Group '%s' has no stability declaration. Add stability: development.", [group.id])
  }
}

# ============================================================
# Rule 2: a2a spans must declare a2a.message_id.
# ============================================================
deny contains violation if {
  group := input.groups[_]
  group.type == "span"
  startswith(group.id, "span.ggen.a2a.")
  is_array(group.attributes)
  attr_refs := {a.ref | a := group.attributes[_]; a.ref}
  not "a2a.message_id" in attr_refs
  violation := {
    "id": "ggen_a2a_missing_message_id",
    "level": "violation",
    "message": sprintf("A2A span '%s' must declare 'a2a.message_id'.", [group.id])
  }
}

# ============================================================
# Rule 3: mcp spans must declare mcp.tool_name.
# ============================================================
deny contains violation if {
  group := input.groups[_]
  group.type == "span"
  startswith(group.id, "span.ggen.mcp.")
  is_array(group.attributes)
  attr_refs := {a.ref | a := group.attributes[_]; a.ref}
  not "mcp.tool_name" in attr_refs
  violation := {
    "id": "ggen_mcp_missing_tool_name",
    "level": "violation",
    "message": sprintf("MCP span '%s' must declare 'mcp.tool_name'.", [group.id])
  }
}

# ============================================================
# Rule 4: yawl spans must declare yawl.workflow_id.
# ============================================================
deny contains violation if {
  group := input.groups[_]
  group.type == "span"
  startswith(group.id, "span.ggen.yawl.")
  is_array(group.attributes)
  attr_refs := {a.ref | a := group.attributes[_]; a.ref}
  not "yawl.workflow_id" in attr_refs
  violation := {
    "id": "ggen_yawl_missing_workflow_id",
    "level": "violation",
    "message": sprintf("YAWL span '%s' must declare 'yawl.workflow_id'.", [group.id])
  }
}

# ============================================================
# Rule 5: pipeline spans must declare pipeline.operation.
# ============================================================
deny contains violation if {
  group := input.groups[_]
  group.type == "span"
  startswith(group.id, "span.ggen.pipeline.")
  is_array(group.attributes)
  attr_refs := {a.ref | a := group.attributes[_]; a.ref}
  not "pipeline.operation" in attr_refs
  violation := {
    "id": "ggen_pipeline_missing_operation",
    "level": "violation",
    "message": sprintf("Pipeline span '%s' must declare 'pipeline.operation'.", [group.id])
  }
}

# ============================================================
# Rule 6: llm spans must declare llm.model.
# ============================================================
deny contains violation if {
  group := input.groups[_]
  group.type == "span"
  startswith(group.id, "span.ggen.llm.")
  is_array(group.attributes)
  attr_refs := {a.ref | a := group.attributes[_]; a.ref}
  not "llm.model" in attr_refs
  violation := {
    "id": "ggen_llm_missing_model",
    "level": "violation",
    "message": sprintf("LLM span '%s' must declare 'llm.model'.", [group.id])
  }
}

# ============================================================
# Rule 7: error spans must declare error.type.
# ============================================================
deny contains violation if {
  group := input.groups[_]
  group.type == "span"
  startswith(group.id, "span.ggen.error.")
  is_array(group.attributes)
  attr_refs := {a.ref | a := group.attributes[_]; a.ref}
  not "error.type" in attr_refs
  violation := {
    "id": "ggen_error_missing_type",
    "level": "violation",
    "message": sprintf("Error span '%s' must declare 'error.type'.", [group.id])
  }
}
