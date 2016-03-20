defmodule RC do
  require Integer
  def even_or_odd2(n) do
    if Integer.is_even(n), do: "#{n} is even", else: "#{n} is odd"
  end
end
