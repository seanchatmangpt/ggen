defmodule Demo.Agents.Echo do
  def init(opts), do: {:ok, opts}
  def handle_message(m), do: {:ok, m}
  def smuggled_admin_backdoor(x), do: x
end
