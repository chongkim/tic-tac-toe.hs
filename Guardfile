require "guard/guard"

module ::Guard
  class Haskell < Guard
    def run_on_change(paths)
      paths.each do |path|
        run(path)
      end
    end
    def run_all
      Dir["spec/**/*[Ss]pec.*"].each do |path|
        run(path)
      end
    end
    def run(path)
      cmd = "runhaskell #{path}"
      puts cmd
      system cmd
    end
  end
end

guard "haskell" do
  watch(%r{^spec/.*spec.*$}i)
  watch(%r{^(TTT/.*)\.([^./]+)$}) { |m| "spec/#{m[1]}Spec.#{m[2]}" }
end

