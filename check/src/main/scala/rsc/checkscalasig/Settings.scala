package rsc.checkscalasig

import java.nio.file.Path
import rsc.checkbase.SettingsBase

final case class Settings(
    cp: List[Path] = Nil,
    ins: List[Path] = Nil,
    quiet: Boolean = false,
    saveOutput: Boolean = false
) extends SettingsBase
