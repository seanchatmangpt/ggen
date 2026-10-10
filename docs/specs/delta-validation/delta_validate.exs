# Delta(G) = 0 translation-validation harness (Elixir/Erlang surface).
#
# Run with the Elixir runtime (the host OTP/Elixir toolchain is the anchor):
#
#   elixir delta_validate.exs <artifact.ex> <expected-surface.txt>
#   [--report <path>]
#
# Compiles the ggen-emitted Elixir artifact to BEAM, reads the Erlang
# `abstract_code` chunk back out of the compiled module (the compiler's own
# parse-back), extracts the exported-function set (the module surface a
# caller can legally touch), and diffs it against the ontology-declared
# surface. Delta(G) = G_recovered \ G_original; the build aborts (exit 1)
# iff Delta(G) is nonempty.
#
# Expected-surface file format (one entry per line, `#` comments):
#   Demo.Agents.Echo.handle_message/1
#   Demo.Agents.Echo.init/1

defmodule DeltaValidate do
  @moduledoc false

  # Compiler-injected surface: emitted by the Elixir/Erlang compiler into
  # every module regardless of the ontology. Part of the projection function
  # pi (spec Section 3.3), not part of Delta(G).
  @compiler_injected ~w(__info__/1 module_info/0 module_info/1)

  # --- parse-back: compile the artifact and recover its surface -----------
  @spec recover(binary) :: {:ok, module(), [binary]} | {:error, term}
  def recover(artifact_path) do
    source = File.read!(artifact_path)

    case Code.compile_string(source, artifact_path) do
      [{mod, beam}] when is_atom(mod) and is_binary(beam) ->
        case :beam_lib.chunks(beam, [:abstract_code]) do
          {:ok, {^mod, abstract_code: {:raw_abstract_v1, forms}}} ->
            {:ok, mod, exports(forms, mod)}

          {:error, _, reason} ->
            {:error, {:abstract_code, reason}}
        end

      other ->
        {:error, {:compile, other}}
    end
  end

  defp exports(forms, mod) do
    exps =
      for {:attribute, _ann, :export, fns} <- forms, fns <- List.wrap(fns) do
        {n, a} = fns
        surface(mod, n, a)
      end

    exps
    |> Enum.reject(fn entry ->
      Enum.any?(@compiler_injected, &String.ends_with?(entry, "." <> &1))
    end)
    |> Enum.sort()
    |> Enum.uniq()
  end

  defp surface(mod, n, a) do
    name = mod |> Atom.to_string() |> String.replace_prefix("Elixir.", "")
    "#{name}.#{n}/#{a}"
  end
end

defmodule DeltaValidate.Main do
  @moduledoc false

  def main(argv) do
    {opts, args, invalid} =
      OptionParser.parse(argv, strict: [report: :string])

    case {invalid, args} do
      {[], [artifact, expected]} ->
        run(artifact, expected, opts[:report])

      {[], _} ->
        usage()
        System.halt(2)
    end
  end

  defp run(artifact, expected_path, report_path) do
    with {:ok, mod, recovered} <- DeltaValidate.recover(artifact),
         {:ok, expected} <- read_surface(expected_path) do
      delta = Enum.sort(recovered -- expected)
      report(mod, recovered, expected, delta, report_path)
    else
      {:error, reason} ->
        IO.puts(:stderr, "REFUSED: #{inspect(reason)}")
        System.halt(3)
    end
  end

  # Delta(G) = G_recovered \ G_original. Abort iff nonempty.
  defp report(mod, recovered, expected, delta, report_path) do
    lines = [
      "delta-validation report",
      "subject: #{mod}",
      "G_original (ontology-declared surface): #{length(expected)} exports",
      "G_recovered (abstract_code surface): #{length(recovered)} exports",
      "Delta(G) = G_recovered \\ G_original = #{inspect(delta)}",
      if(delta == [],
        do: "VERDICT: ALIVE (Delta(G) = 0)",
        else: "VERDICT: ABORT (Delta(G) nonempty)"
      )
    ]

    out = Enum.join(lines, "\n") <> "\n"

    case report_path do
      nil -> IO.write(out)
      path ->
        File.write!(path, out)
        IO.write(out)
    end

    if delta == [] do
      :erlang.halt(0, flush: true)
    else
      System.halt(1)
    end
  end

  defp read_surface(path) do
    entries =
      path
      |> File.read!()
      |> String.split("\n")
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == "" or String.starts_with?(&1, "#")))
      |> Enum.sort()
      |> Enum.uniq()

    {:ok, entries}
  end

  defp usage do
    IO.puts(:stderr, "usage: escript delta_validate.exs <artifact.ex> <expected-surface.txt>"
                 <> " [--report <path>]")
  end
end

DeltaValidate.Main.main(System.argv())
