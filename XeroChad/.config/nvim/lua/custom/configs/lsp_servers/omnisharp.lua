local omnisharp_path = vim.fn.stdpath("data") .. "/mason/packages/omnisharp/libexec/OmniSharp.dll"

return {
  cmd = { "dotnet", omnisharp_path},
  enable_editorconfig_support = true,
  enable_ms_build_load_projects_on_demand = false,
  enable_roslyn_analyzers = false,
  organize_imports_on_format = true,
  enable_import_completion = true,
  sdk_include_prereleases = true,
  analyze_open_documents_only = false,
}
